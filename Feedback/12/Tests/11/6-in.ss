(
	(module MTWO
		(class CTWO ())
	)
	(tmodule MONE
		(timport MTWO (() ()))
		(timport MTWO (((x Number)) ()))
		(class CONE ())
		(() ())
	)
	1.0
)