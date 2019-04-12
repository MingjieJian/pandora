      subroutine BELTERM
     $(LU)
C
C     Rudolf Loeser, 2004 Apr 09
C---- Tells about statistical equilibrium and weight matrices,
C     for TREMBLE.
C     !DASH
      save
C     !DASH
      integer LU
C     !DASH
      external LINER, HI, BYE
C
      call HI ('BELTERM')
C     !BEG
      call LINER (2, LU)
      write (LU,100)
  100 format(' ','S T A T I S T I C A L   E Q U I L I B R I U M'//
     $       ' ','Five different formulations of the statistical ',
     $           'equilibrium calculations have been provided;'/
     $       ' ','they are selected by input parameter METSE(u,l). ',
     $           'They have names as follows:'/
     $       ' ','     NOVA         metse = 0'/
     $       ' ','     COMPLEX-U    metse = 1'/
     $       ' ','     COMPLEX-L    metse = 2'/
     $       ' ','     CHAIN        metse = 3'/
     $       ' ','     VAMOS        metse = 4'/
     $       ' ','When METSW = on, an automatic procedure attempts ',
     $           'to use a "better" method if the current selection ',
     $           'gives "bad" results;'/
     $       ' ','we have found that this leads to confusion and ',
     $           'tend not to use it.')
      call LINER (1, LU)
      write (LU,101)
  101 format(' ','METSE = 1 seems to be a good general initial ',
     $           'choice, and is recommended when problems arise, ',
     $           'and for weak transitions.'/
     $       ' ','METSE = 3 seems to be a good choice for ',
     $           'transitions with lower level = 1.'/
     $       ' ','METSE = 2 is usually unstable and should not be ',
     $           'used routinely.'/
     $       ' ','Nova has the advantage of using only RHOs from ',
     $           'other transitions, while Chain uses both RHOs and ',
     $           'b-ratios. Vamos is an'/
     $       ' ','advanced method that is the only one that works ',
     $           'in some difficult problems, but is not always '
     $           'best for routine use.')
C     !EJECT
      call LINER (2, LU)
      write (LU,102)
  102 format(' ','W E I G H T   M A T R I C E S'//
     $       ' ','Several different formulations of the weight ',
     $           'matrix (Lambda operator) calculation have been ',
     $           'provided;'/
     $       ' ','for the LSF calculation they are selected by the ',
     $           'input parameter YLINE(u,l). They have names as ',
     $           'follows:'/
     $       ' ','     QR           0 .le. yline .le. 1'/
     $       ' ','     RT           yline = -1'/
     $       ' ','     GR           yline = -3'/
     $       ' ','The QR and RT methods use analytic angle ',
     $           'integration, while GR requires numerical ',
     $           'integration over all specified rays.'/
     $       ' ','(Various other input parameters serve to control ',
     $           'these calculations.) We have found that RT is ',
     $           'useable in most situations'/
     $       ' ','(it is used by default); but GR is required when ',
     $           'spherical geometry is used and when mass flows ',
     $           'affect the source function.')
C
      call LINER (1, LU)
      write (LU,200)
  200 format(' ',116X,'2006 Mar 06')
C     !END
      call BYE ('BELTERM')
C
      return
      end
