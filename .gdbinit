unset-e EDITOR
unset-e MAILER
unset-e MAN
unset-e MORE
unset-e NAME
unset-e ORGANIZATION
unset-e PAGER
unset-e TERMCAP

define ptc
output (enum tree_code) $.shared.code
echo \n
end

document ptc
Print the tree-code of the tree node that is $.
end

define pdn
output $.decl.name->identifier.pointer
echo \n
end

document pdn
Print the name of the decl-node that is $.
end

define prc
output (enum rtx_code) $.code
echo \n
end

document prc
Print the rtx-code of the rtx that is $.
end

define pi
print $.fld[0].rtx@6
end

document pi
Print the fields of an instruction.
end
