      subroutine ZAREC
     $(IMX,IDM,JMX,CKL,COP,OPAC,WRK)
C
C     Rudolf Loeser, 1983 Feb 18
C---- Prints debug data for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CKL, COP, OPAC, WRK
      integer IDM, IMX, JMX, LUEO
      character LINE*127
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LABFIL, VECOUT, COMPACT, ARROUT, HI, BYE
C
C               COP(IDM,JMX), CKL(IDM,JMX), OPAC(JMX), WRK(IDM,JMX)
      dimension COP(*),       CKL(*),       OPAC(*),   WRK(*)
C
      call HI ('ZAREC')
C     !BEG
      call LABFIL  ('COP', LINE)
      call VECOUT  (LUEO, COP, JMX, LINE)
C
      call COMPACT (CKL , IMX, IDM, JMX, WRK)
      call LABFIL  ('KL'  , LINE)
      call ARROUT  (LUEO, WRK, IMX, JMX, LINE)
C
      call COMPACT (OPAC, IMX, IDM, JMX, WRK)
      call LABFIL  ('OPAC', LINE)
      call ARROUT  (LUEO, WRK, IMX, JMX, LINE)
C     !END
      call BYE ('ZAREC')
C
      return
      end
