# mtr-toolkit

mtr-toolkit: A multi-target regression (MTR) toolkit in R!

This project implements current state-of-the-art MTR solutions, as well as, new methods proposed by Saulo Martiello Mastelini (in conjunction with other researchers, e.g., Sylvio Barbon Jr. and Everton Jose Santana).

Some of the solutions are, currently, under tests. A documentation (possibly) will be created as the codes are improved and new publications are obtained.

Currently, only local approaches are supported. The currently implemented methods are (MTR methods proposed by Mastelini are marked with \*):

- ST: Single-target
- MTRS: Multi-target Regressor Stacking (a.k.a. SST -- Stacked Single-target)
- ERC: Ensemble of Regressor Chains
- DSTARS: Deep Structure for Tracking Asynchronous Regressor Stacking (\*)
- DRS: Deep Regressor Stacking (\*)
- MTAS: Multi-target Augmented Stacking (\*)
- MOTC: Multi-output Tree Chaining (\*)
- ORC: Optimum Regressor Chains (\*)
- MTSG: Multi-target Stacked Generalization (\*)
- ESR: Ensemble of Stacked Regressors (\*)


Both SST (MTRS) and ERC were proposed by Spyromitros-Xioufis et. al (2016) and can be found in:

- Spyromitros-Xioufis, E., Tsoumakas, G., Groves, W. and Vlahavas, I., 2016. Multi-target regression via input space expansion: treating targets as inputs. Machine Learning, 104(1), pp.55-98.

*** I will add the corresponding papers for our methods ASAP (the ones that were already published). ***
