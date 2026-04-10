# FAQ

This page collects answers to questions we have received about
gaplightr.

------------------------------------------------------------------------

## Q: Does gaplightr estimate LAI like GLA does?

No. gaplightr does not currently produce Leaf Area Index (LAI)
estimates. LAI is fundamentally a 3D quantity that cannot be reliably
resolved from 2D hemispherical imagery alone, because doing so requires
independent estimates of foliage clumping and the projected area of all
woody material.

------------------------------------------------------------------------

## Q: How is the Light Penetration Index (LPI) calculated, and does GLA provide it?

LPI is not provided by the original GLA software. gaplightr defines it
as percent transmitted global solar radiation expressed as a fraction:
divide percent transmitted global by 100. GLA computes percent
transmitted direct, diffuse, and global solar radiation, so LPI can be
derived from GLA output using the same formula.
