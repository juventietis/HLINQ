--Original
DoE [
	BindS (VarP age1_13) (DoE [
		BindS (VarP person_16) (VarE people),
		NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_16))) (VarE ==) 
			(Just (LitE (StringL "Drew")))))),
		NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE age) (VarE person_16))))]),
	BindS (VarP age2_14) (DoE [BindS (VarP person_18) (VarE people),
		NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_18))) (VarE ==) 
			(Just (LitE (StringL "Jen")))))),
		NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE age) (VarE person_18))))]),
	NoBindS (DoE [BindS (VarP person_21) (VarE people),
		NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (InfixE (Just (VarE age1_13)) (VarE <=) 
			(Just (AppE (VarE age) (VarE person_21))))) (VarE &&) (Just (InfixE (Just (AppE (VarE age) (VarE person_21))) 
			(VarE <) (Just (VarE age2_14))))))),
		NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE name) (VarE person_21))))])]

-- After symbolic normalisation
DoE [
	BindS (VarP person_38) (VarE people),
	NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_38))) (VarE ==)
	 (Just (LitE (StringL "Jen")))))),
	BindS (VarP person_40) (VarE people),
	NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_40))) (VarE ==)
	 (Just (LitE (StringL "Drew")))))),
	NoBindS (DoE [
		BindS (VarP person_43) (VarE people),
		NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (InfixE (Just (AppE (VarE age) (VarE person_38)))
		 (VarE <=) (Just (AppE (VarE age) (VarE person_43))))) (VarE &&) (Just (InfixE (Just (AppE (VarE age)
		  (VarE person_43))) (VarE <) (Just (AppE (VarE age) (VarE person_40)))))))),
		NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE name) (VarE person_43))))])]

-- After flattening
DoE [
	BindS (VarP person_49) (VarE people),
	NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_49))) (VarE ==) 
		(Just (LitE (StringL "Jen")))))),
	BindS (VarP person_51) (VarE people),
	NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_51))) (VarE ==) 
		(Just (LitE (StringL "Drew")))))),
	BindS (VarP person_54) (VarE people),
	NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (InfixE (Just (AppE (VarE age) (VarE person_49))) 
		(VarE <=) (Just (AppE (VarE age) (VarE person_54))))) (VarE &&) (Just (InfixE (Just (AppE (VarE age) 
			(VarE person_54))) (VarE <) (Just (AppE (VarE age) (VarE person_51)))))))),
	NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE name) (VarE person_54))))]

-- Guards
[NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_71))) (VarE ==) 
	(Just (LitE (StringL "Jen")))))),
NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_73))) (VarE ==) 
	(Just (LitE (StringL "Drew")))))),
NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (InfixE (Just (AppE (VarE age) (VarE person_71)))
 (VarE <=) (Just (AppE (VarE age) (VarE person_76))))) (VarE &&) (Just (InfixE (Just (AppE (VarE age) (VarE person_76))) 
 (VarE <) (Just (AppE (VarE age) (VarE person_73))))))))]

[NoBindS (InfixE (Just (VarE guard)) (VarE $) 
	(Just (InfixE (Just (InfixE (Just (InfixE (Just (AppE (VarE name) (VarE person_115))) (VarE ==) (Just (LitE (StringL "Jen"))))) 
		(VarE &&) 
	(Just (InfixE (Just (AppE (VarE name) (VarE person_117))) (VarE ==) (Just (LitE (StringL "Drew"))))))) 
		(VarE &&)
	(Just (InfixE (Just (InfixE (Just (AppE (VarE age) (VarE person_115))) (VarE <=) (Just (AppE (VarE age) (VarE person_120)))))
		(VarE &&) 
	(Just (InfixE (Just (AppE (VarE age) (VarE person_120))) (VarE <) (Just (AppE (VarE age) (VarE person_117))))))))))]

DoE [
	BindS (VarP person_126) (VarE people),
	BindS (VarP person_128) (VarE people),
	BindS (VarP person_131) (VarE people),
	NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE name) (VarE person_131)))),
	NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (InfixE (Just (InfixE (Just (AppE (VarE name) (VarE person_126))) (VarE ==) (Just (LitE (StringL "Jen"))))) (VarE &&) (Just (InfixE (Just (AppE (VarE name) (VarE person_128))) (VarE ==) (Just (LitE (StringL "Drew"))))))) (VarE &&) (Just (InfixE (Just (InfixE (Just (AppE (VarE age) (VarE person_126))) (VarE <=) (Just (AppE (VarE age) (VarE person_131))))) (VarE &&) (Just (InfixE (Just (AppE (VarE age) (VarE person_131))) (VarE <) (Just (AppE (VarE age) (VarE person_128))))))))))]


-- Satisfies
AppE (LamE [VarP p_109] (
	DoE [
		BindS (VarP person_110) (VarE people),
		NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (AppE (VarE p_109) (AppE (VarE age) (VarE person_110))))),
		NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE name) (VarE person_110))))])) (
LamE [VarP x_111] (InfixE (Just (InfixE (Just (LitE (IntegerL 30))) (VarE <=) (Just (VarE x_111)))) (VarE &&) (Just (InfixE (Just (VarE x_111)) (VarE <) (Just (LitE (IntegerL 40)))))))

DoE [
	BindS (VarP person_113) (VarE people),
	NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE name) (VarE person_113)))),
	NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (AppE (LamE [VarP x_114] (InfixE (Just (InfixE (Just (LitE (IntegerL 30))) 
		(VarE <=) (Just (VarE x_114)))) (VarE &&) (Just (InfixE (Just (VarE x_114)) (VarE <) (Just (LitE (IntegerL 40))))))) 
	(AppE (VarE age) (VarE person_113)))))]













DoE [
	BindS (VarP person_170) (VarE people),
	BindS (VarP age1_167) (DoE [
		NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_170))) (VarE ==) 
			(Just (LitE (StringL "Drew")))))),
		NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE age) (VarE person_170)))),
		BindS (VarP person_172) (VarE people),BindS (VarP age2_168) (DoE [
			NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (AppE (VarE name) (VarE person_172))) (VarE ==)
			 (Just (LitE (StringL "Jen")))))),
			NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE age) (VarE person_172)))),
			NoBindS (DoE [
				BindS (VarP person_175) (VarE people),
				NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (InfixE (Just (VarE age1_167)) (VarE <=) 
					(Just (AppE (VarE age) (VarE person_175))))) (VarE &&) (Just (InfixE (Just (AppE (VarE age) 
						(VarE person_175))) (VarE <) (Just (VarE age2_168))))))),
				NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (AppE (VarE name) (VarE person_175))))])])])]


-- Predicates
Doe [
	BindS (VarP person_35) (VarE people),
	NoBindS (InfixE (Just (VarE return)) (VarE $) (Just (VarE name))),
	NoBindS (InfixE (Just (VarE guard)) (VarE $) (Just (InfixE (Just (InfixE (Just (LitE (IntegerL 30))) (VarE <) (Just (VarE age)))) (VarE &&)
	 (Just (InfixE (Just (AppE (VarE age) (VarE person_35))) (VarE <=) (Just (LitE (IntegerL 40))))))))]
