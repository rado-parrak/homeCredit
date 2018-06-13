=============================================================================================
		             ==== Variable types and their pre-fixes ====
=============================================================================================

Intro:
The idea is to use prefixes to identify standard statistical variable (not data!) types, for
easier manipulation and model building.

Let's stick to the standard variable types used in data-mining (unless someone has a better idea)

Variable & Prefixes: 

1. T_		| TARGET 								(used in modelling)
2. ID_		| Identifier								(NOT used in modelling)	
2. DT_ 		| DATE 									(NOT used in modelling)
3. I_		| INDICATOR, a.k.a. flag, a.k.a special case of categorical variable	(used in modelling)
4. CN_		| CATEGORICAL NOMINAL variable (except for the INDICATORs)		(used in modelling)
5. CO_		| CATEGORICAL ORDINAL variable (except for the INDICATORs)		(used in modelling)
6. Q_		| QUANTITATIVE variable (a.k.a interval)				(used in modelling)
7. B_		| BUCKETED/BINNED feature						(used in modelling)
