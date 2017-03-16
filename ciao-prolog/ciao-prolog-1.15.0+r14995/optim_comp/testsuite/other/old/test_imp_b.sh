../runme.sh --uc && touch ptoc_imp_b.pl && ../runme.sh --try --update prolog_bin ptoc_imp_b && { ciaodump --module prolog_native ptoc_imp_b > k.txt; } && diff k.txt k.txt-orig && echo ok
