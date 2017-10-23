/* Optimize jump instructions, for GNU compiler.
   Copyright (C) 1987 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


/* This is the jump-optimization pass of the compiler.
   It is run two or three times: once before cse, sometimes once after cse,
   and once after reload (before final).

   jump_optimize deletes unreachable code and labels that are not used.
   It also deletes jumps that jump to the following insn,
   and simplifies jumps around unconditional jumps and jumps
   to unconditional jumps.

   Each CODE_LABEL has a count of the times it is used
   stored in the LABEL_NUSES internal field, and each JUMP_INSN
   has one label that it refers to stored in the
   JUMP_LABEL internal field.  With this we can detect labels that
   become unused because of the deletion of all the jumps that
   formerly used them.  The JUMP_LABEL info is sometimes looked
   at by later passes.

   Optionally, cross-jumping can be done.  Currently it is done
   only the last time (when after reload and before final).
   In fact, the code for cross-jumping now assumes that register
   allocation has been done, since it uses `rtx_renumbered_equal_p'.

   Jump optimization is done after cse when cse's constant-propagation
   causes jumps to become unconditional or to be deleted.

   Unreachable loops are not detected here, because the labels
   have references and the insns appear reachable from the labels.
   find_basic_blocks in flow.c finds and deletes such loops.

   The subroutines delete_insn, redirect_jump, invert_jump, next_real_insn
   and prev_real_insn are used from other passes as well.  */

#include "config.h"
#include "rtl.h"

/* ??? Eventually must record somehow the labels used by jumps
   from nested functions.  */
/* Pre-record the next or previous real insn for each label?
   No, this pass is very fast anyway.  */
/* Condense consecutive labels?
   This would make life analysis faster, maybe.  */
/* Optimize jump y; x: ... y: jumpif... x?
   Don't know if it is worth bothering with.  */
/* Optimize two cases of conditional jump to conditional jump?
   This can never delete any instruction or make anything dead,
   or even change what is live at any point.
   So perhaps let combiner do it.  */

void delete_insn ();
void redirect_jump ();
void invert_jump ();
rtx next_real_insn ();
rtx prev_real_insn ();

static void mark_jump_label ();
static void delete_jump ();
static void invert_exp ();
static void redirect_exp ();
static rtx follow_jumps ();
static int tension_vector_labels ();
static void find_cross_jump ();

/* Delete no-op jumps and optimize jumps to jumps
   and jumps around jumps.
   Delete unused labels and unreachable code.
   If CROSS_JUMP is nonzero, detect matching code
   before a jump and its destination and unify them.  */

void
jump_optimize (f, cross_jump)
     rtx f;
{
  register rtx insn;
  int changed;

  /* Initialize LABEL_NUSES and JUMP_LABEL fields.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == CODE_LABEL)
	LABEL_NUSES (insn) = 0;
      if (GET_CODE (insn) == JUMP_INSN)
	JUMP_LABEL (insn) = 0;
    }

  /* Delete insns following barriers, up to next label.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == BARRIER)
      while (1)
	{
	  register rtx next = NEXT_INSN (insn);
	  if (next == 0 || GET_CODE (next) == CODE_LABEL)
	    break;
	  if (GET_CODE (next) == NOTE)
	    insn = next;
	  else
	    delete_insn (next);
	}

  /* Mark the label each jump jumps to.
     Count uses of CODE_LABELs.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == JUMP_INSN)
      {
	mark_jump_label (PATTERN (insn), insn);
      }

  /* Delete all labels already not referenced.  */

  for (insn = f; insn; )
    {
      register rtx next = NEXT_INSN (insn);
      if (GET_CODE (insn) == CODE_LABEL && LABEL_NUSES (insn) == 0)
	{
	  delete_insn (insn);
	  next = NEXT_INSN (PREV_INSN (insn));
	}
      insn = next;
    }

  /* Now iterate optimizing jumps until nothing changes over one pass.  */
  changed = 1;
  while (changed)
    {
      register rtx next;
      changed = 0;

      for (insn = f; insn; insn = next)
	{
	  next = NEXT_INSN (insn);
	  if (GET_CODE (insn) == JUMP_INSN)
	    {
	      if (GET_CODE (PATTERN (insn)) == ADDR_VEC)
		changed |= tension_vector_labels (PATTERN (insn), 0);
	      if (GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
		changed |= tension_vector_labels (PATTERN (insn), 1);
	    }
	  if (GET_CODE (insn) == JUMP_INSN && JUMP_LABEL (insn))
	    {
	      register rtx reallabelprev = prev_real_insn (JUMP_LABEL (insn));

	      /* Detect Jump to following insn.  */
	      if (reallabelprev == insn && condjump_p (insn))
		{
		  reallabelprev = PREV_INSN (insn);
		  delete_jump (insn);
		  changed = 1;
		}
	      /* Detect jumping over an unconditional jump.  */
	      else if (reallabelprev != 0
		       && GET_CODE (reallabelprev) == JUMP_INSN
		       && prev_real_insn (reallabelprev) == insn
		       && no_labels_between_p (insn, reallabelprev)
		       && simplejump_p (reallabelprev))
		{
		  /* Delete the original unconditional jump (and barrier).  */
		  /* But don't let its destination go with it.  */
		  ++LABEL_NUSES (JUMP_LABEL (reallabelprev));
		  delete_insn (reallabelprev);
		  /* Now change the condition, and make it go to the
		     place the deleted jump went to.
		     This may cause the label after the deletion to go away.
		     But now that the unconditional jump and its barrier
		     are gone, that is ok.  */
		  invert_jump (insn, JUMP_LABEL (reallabelprev));
		  --LABEL_NUSES (JUMP_LABEL (reallabelprev));
		  next = insn;
		  changed = 1;
		}
	      else
		{
		  /* Detect a jump to a jump.  */
		  {
		    register rtx nlabel = follow_jumps (JUMP_LABEL (insn));
		    if (nlabel != JUMP_LABEL (insn))
		      {
			redirect_jump (insn, nlabel);
			changed = 1;
			next = insn;
		      }
		  }
		  /* Now that the jump has been tensioned,
		     try cross jumping: check for identical code
		     before the jump and before its target label. */
		  if (cross_jump && condjump_p (insn))
		    {
		      rtx newjpos, newlpos;

		      find_cross_jump (insn, JUMP_LABEL (insn),
				       &newjpos, &newlpos);
		      if (newjpos != 0)
			{
			  register rtx label;
			  /* Find an existing label at this point
			     or make a new one if there is none.  */
			  label = PREV_INSN (newlpos);
			  if (GET_CODE (label) != CODE_LABEL)
			    {
			      label = gen_label_rtx ();
			      emit_label_after (label, PREV_INSN (newlpos));
			      LABEL_NUSES (label) = 0;
			    }
			  /* Make the same jump insn jump to the new point.  */
			  redirect_jump (insn, label);
			  /* Delete the matching insns before the jump.  */
			  newjpos = PREV_INSN (newjpos);
			  while (NEXT_INSN (newjpos) != insn)
			    /* Don't delete line numbers.  */
			    if (GET_CODE (NEXT_INSN (newjpos)) != NOTE)
			      delete_insn (NEXT_INSN (newjpos));
			    else
			      newjpos = NEXT_INSN (newjpos);
			  changed = 1;
			  next = insn;
			}
		    }
		}
	    }
	}
    }
}

/* Compare the instructions before insn E1 with those before E2.
   Find the longest possible equivalent sequences
   and store the first insns of those sequences into *F1 and *F2.
   Store zero there if no equivalent preceding instructions are found.

   We give up if we find a label in stream 1.
   Actually we could transfer that label into stream 2.  */

static void
find_cross_jump (e1, e2, f1, f2)
     rtx e1, e2;
     rtx *f1, *f2;
{
  register rtx i1 = e1, i2 = e2;
  register rtx p1, p2;
  int had_pairs = 0;
  int nontrivial = 0;

  *f1 = 0;
  *f2 = 0;

  /* This is how it really should work:

     Scan backward one insn at a time
     and maintain the pair table as showing all corresponding
     registers that are not the same number.
     Pairs are inserted when the two insns being compared
     use different registers.
     Pairs are removed when we find two insns being compared
     each setting the appropriate member of the pair.
     Any other differences disqualify the insns, and we stop.
     When the pair table is empty after scanning an insn,
     that insn is the beginning of a sequence of equivalent insns.

     But what we really do is give up on any sign of a mismatch.
     We can't make the pair-table win because it is incorrect
     if anything AFTER the original label (E2) refers to those
     mismatched registers.  (That is, if they aren't really temps.)  */

  while (1)
    {
      i1 = PREV_INSN (i1);
      while (i1 && GET_CODE (i1) == NOTE)
	i1 = PREV_INSN (i1);

      i2 = PREV_INSN (i2);
      while (i2 && (GET_CODE (i2) == NOTE || GET_CODE (i2) == CODE_LABEL))
	i2 = PREV_INSN (i2);

      if (i1 == 0 || GET_CODE (i1) == CODE_LABEL || GET_CODE (i1) == BARRIER)
	break;

      if (i2 == 0 || GET_CODE (i1) != GET_CODE (i2))
	break;

      /* This is the simplest way to solve the problem
	 of splitting the code between a (SET (CC0) ...)
	 and a following conditional jump: don't match jumps.
	 Combine would see the (SET (CC0) ...) and an unconditional jump
	 and delete the former.  */
      if (GET_CODE (i1) == JUMP_INSN)
	break;

      p1 = PATTERN (i1);
      p2 = PATTERN (i2);

      if (GET_CODE (p1) != GET_CODE (p2))
	break;

      if (!rtx_renumbered_equal_p (p1, p2))
	break;

      if (GET_CODE (p1) != USE && GET_CODE (p1) != CLOBBER)
	nontrivial = 1;

      if (nontrivial)
	*f1 = i1, *f2 = i2;
    }
}

/* Return 1 if INSN is an unconditional jump and nothing else.  */

static int
simplejump_p (insn)
     rtx insn;
{
  register rtx x = PATTERN (insn);
  if (GET_CODE (x) != SET)
    return 0;
  if (GET_CODE (SET_DEST (x)) != PC)
    return 0;
  if (GET_CODE (SET_SRC (x)) != LABEL_REF)
    return 0;
  return 1;
}

/* Return nonzero if INSN is a (possibly) conditional jump
   and nothing more.  */

static int
condjump_p (insn)
     rtx insn;
{
  register rtx x = PATTERN (insn);
  if (GET_CODE (x) != SET)
    return 0;
  if (GET_CODE (SET_DEST (x)) != PC)
    return 0;
  if (GET_CODE (SET_SRC (x)) == LABEL_REF)
    return 1;
  if (GET_CODE (SET_SRC (x)) != IF_THEN_ELSE)
    return 0;
  if (XEXP (SET_SRC (x), 2) == pc_rtx
      && GET_CODE (XEXP (SET_SRC (x), 1)) == LABEL_REF)
    return 1;
  if (XEXP (SET_SRC (x), 1) == pc_rtx
      && GET_CODE (XEXP (SET_SRC (x), 2)) == LABEL_REF)
    return 1;
  return 0;
}

/* Return 1 if in between BEG and END there is no CODE_LABEL insn.  */

int
no_labels_between_p (beg, end)
     rtx beg, end;
{
  register rtx p;
  for (p = beg; p != end; p = NEXT_INSN (p))
    if (GET_CODE (p) == CODE_LABEL)
      return 0;
  return 1;
}

/* Return the last INSN, CALL_INSN or JUMP_INSN before LABEL;
   or 0, if there is none.  */

rtx
prev_real_insn (label)
     rtx label;
{
  register rtx insn = PREV_INSN (label);
  register RTX_CODE code;

  while (1)
    {
      if (insn == 0)
	return 0;
      code = GET_CODE (insn);
      if (code == INSN || code == CALL_INSN || code == JUMP_INSN)
	break;
      insn = PREV_INSN (insn);
    }

  return insn;
}

/* Return the next INSN, CALL_INSN or JUMP_INSN after LABEL;
   or 0, if there is none.  */

rtx
next_real_insn (label)
     rtx label;
{
  register rtx insn = NEXT_INSN (label);
  register RTX_CODE code;

  while (1)
    {
      if (insn == 0)
	return insn;
      code = GET_CODE (insn);
      if (code == INSN || code == CALL_INSN || code == JUMP_INSN)
	break;
      insn = NEXT_INSN (insn);
    }

  return insn;
}

/* Follow any unconditional jump at LABEL;
   return the ultimate label reached by any such chain of jumps.
   If LABEL is not followed by a jump, return LABEL.  */

static rtx
follow_jumps (label)
     rtx label;
{
  register rtx insn;
  register rtx next;
  register rtx value = label;
  register int depth;

  for (depth = 0;
       (depth < 10
	&& (insn = next_real_insn (value)) != 0
	&& GET_CODE (insn) == JUMP_INSN
	&& JUMP_LABEL (insn) != 0
	&& (next = NEXT_INSN (insn))
	&& GET_CODE (next) == BARRIER);
       depth++)
    {
      value = JUMP_LABEL (insn);
    }
  return value;
}

/* Assuming that field IDX of X is a vector of label_refs,
   replace each of them by the ultimate label reached by it.
   Return nonzero if a change is made.  */

static int
tension_vector_labels (x, idx)
     register rtx x;
     register int idx;
{
  int changed = 0;
  register int i;
  for (i = XVECLEN (x, idx) - 1; i >= 0; i--)
    {
      register rtx olabel = XEXP (XVECEXP (x, idx, i), 0);
      register rtx nlabel = follow_jumps (olabel);
      if (nlabel != olabel)
	{
	  XEXP (XVECEXP (x, idx, i), 0) = nlabel;
	  ++LABEL_NUSES (nlabel);
	  if (--LABEL_NUSES (olabel) == 0)
	    delete_insn (olabel);
	  changed = 1;
	}
    }
  return changed;
}

/* Find all CODE_LABELs referred to in X,
   and increment their use counts.
   Also store one of them in JUMP_LABEL (INSN).  */

static void
mark_jump_label (x, insn)
     register rtx x;
     rtx insn;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register char *fmt;

  if (code == LABEL_REF)
    {
      register rtx label = XEXP (x, 0);
      if (GET_CODE (label) != CODE_LABEL)
	return;
      ++LABEL_NUSES (label);
      JUMP_LABEL (insn) = label;
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	mark_jump_label (XEXP (x, i), insn);
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    mark_jump_label (XVECEXP (x, i, j), insn);
	}
    }
}

/* If all INSN does is set the pc, delete it,
   and delete the insn that set the condition codes for it
   if that's what the previous thing was.  */

static void
delete_jump (insn)
     rtx insn;
{
  register rtx x = PATTERN (insn);
  register rtx prev;

  if (GET_CODE (x) == SET
      && GET_CODE (SET_DEST (x)) == PC)
    {
      prev = PREV_INSN (insn);
      delete_insn (insn);
      /* We assume that at this stage
	 CC's are always set explicitly
	 and always immediately before the jump that
	 will use them.  So if the previous insn
	 exists to set the CC's, delete it.  */
      while (prev && GET_CODE (prev) == NOTE)
	prev = PREV_INSN (prev);
      if (prev && GET_CODE (prev) == INSN
	  && GET_CODE (PATTERN (prev)) == SET
	  && SET_DEST (PATTERN (prev)) == cc0_rtx)
	delete_insn (prev);
    }
}

/* Delete insn INSN from the chain of insns
   and also update whatever redundant data needs
   to be updated as a result.  May delete more insns elsewhere.  */

void
delete_insn (insn)
  register rtx insn;
{
  register rtx next = NEXT_INSN (insn);
  register rtx prev = PREV_INSN (insn);

  /* If instruction is followed by a barrier,
     delete the barrier too.  */

  if (next != 0 && GET_CODE (next) == BARRIER)
    next = NEXT_INSN (next);

  /* Patch out INSN (and the barrier if any) */

  if (prev)
    NEXT_INSN (prev) = next;

  if (next)
    PREV_INSN (next)= prev;

  /* If deleting a jump, decrement the count of the label,
     and delete the label if it is now unused.  */

  if (GET_CODE (insn) == JUMP_INSN && JUMP_LABEL (insn))
    if (--LABEL_NUSES (JUMP_LABEL (insn)) == 0)
      delete_insn (JUMP_LABEL (insn));

  while (prev && GET_CODE (prev) == NOTE)
    prev = PREV_INSN (prev);

  /* After deleting a label, maybe delete code that follows it.  */
  if (GET_CODE (insn) == CODE_LABEL && prev
      && GET_CODE (prev) == BARRIER)
    {
      register RTX_CODE code;
      while (next != 0
	     && ((code = GET_CODE (next)) == INSN
		 || code == JUMP_INSN || code == CALL_INSN
		 || code == NOTE))
	{
	  if (code == NOTE)
	    prev = next;
	  else
	    /* Note: if this deletes a jump, it can cause more
	       deletion of unreachable code, after a different label.
	       But the two levels of deletion won't interfere,
	       because in that case there must be a BARRIER between them,
	       and we never delete BARRIERs.  */
	    delete_insn (next);
	  next = NEXT_INSN (prev);
	}
    }
}

/* Invert the condition of the jump JUMP, and make it jump
   to label NLABEL instead of where it jumps now.  */

void
invert_jump (jump, nlabel)
     rtx jump, nlabel;
{
  register rtx olabel = JUMP_LABEL (jump);
  invert_exp (PATTERN (jump), olabel, nlabel);
  JUMP_LABEL (jump) = nlabel;
  ++LABEL_NUSES (nlabel);
  INSN_CODE (jump) = -1;

  if (--LABEL_NUSES (olabel) == 0)
    delete_insn (olabel);
}

/* Invert the jump condition of rtx X,
   and replace OLABEL with NLABEL throughout.  */

static void
invert_exp (x, olabel, nlabel)
     rtx x;
     rtx olabel, nlabel;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register char *fmt;

  if (code == IF_THEN_ELSE)
    {
      /* Inverting the jump condition of an IF_THEN_ELSE
	 means exchanging the THEN-part with the ELSE-part.  */
      register rtx tem = XEXP (x, 1);
      XEXP (x, 1) = XEXP (x, 2);
      XEXP (x, 2) = tem;
    }

  if (code == LABEL_REF)
    {
      if (XEXP (x, 0) == olabel)
	XEXP (x, 0) = nlabel;
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	invert_exp (XEXP (x, i), olabel, nlabel);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    invert_exp (XVECEXP (x, i, j), olabel, nlabel);
	}
    }
}

/* Make jump JUMP jump to label NLABEL instead of where it jumps now.
   If the old jump target label is unused as a result,
   it and the code following it may be deleted.  */

void
redirect_jump (jump, nlabel)
     rtx jump, nlabel;
{
  register rtx olabel = JUMP_LABEL (jump);

  if (nlabel == olabel)
    return;

  redirect_exp (PATTERN (jump), olabel, nlabel);
  JUMP_LABEL (jump) = nlabel;
  ++LABEL_NUSES (nlabel);
  INSN_CODE (jump) = -1;

  if (--LABEL_NUSES (olabel) == 0)
    delete_insn (olabel);
}

/* Throughout the rtx X,
   alter (LABEL_REF OLABEL) to (LABEL_REF NLABEL).  */

static void
redirect_exp (x, olabel, nlabel)
     rtx x;
     rtx olabel, nlabel;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register char *fmt;

  if (code == LABEL_REF)
    {
      if (XEXP (x, 0) == olabel)
	XEXP (x, 0) = nlabel;
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	redirect_exp (XEXP (x, i), olabel, nlabel);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    redirect_exp (XVECEXP (x, i, j), olabel, nlabel);
	}
    }
}
