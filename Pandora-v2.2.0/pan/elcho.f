      subroutine ELCHO
     $(IMX,IDM,JMX,CKL,COP,OPAC,WRK,DUMP)
C
C     Rudolf Loeser, 1983 Feb 18
C---- Computes total opacity along a ray.
C     (This is version 2 of ELCHO.)
C     !DASH
      save
C     !DASH
      real*8 CKL, COP, OPAC, WRK
      integer I, IDM, IMX, J, JMX
      logical DUMP
C     !DASH
      external ZAREC, HI, BYE
C
C               CKL(IDM,JMX), OPAC(IDM,JMX), COP(JMX), WRK(IDM,JMX)
      dimension CKL(IDM,*),   OPAC(IDM,*),   COP(*),   WRK(*)
C
      call HI ('ELCHO')
C     !BEG
      do 101 I = 1,IMX
        do 100 J = 1,JMX
          OPAC(I,J) = COP(J)+CKL(I,J)
  100   continue
  101 continue
C
      if(DUMP) then
        call ZAREC (IMX, IDM, JMX, CKL, COP, OPAC, WRK)
      end if
C     !END
      call BYE ('ELCHO')
C
      return
      end
