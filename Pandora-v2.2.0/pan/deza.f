      subroutine DEZA
     $(NO,IM,JM,A,CODSRW)
C
C     Rudolf Loeser, 1983 Mar 14
C---- Output routine - special version of "OUTOUT".
C     (This is version 3 of DEZA.)
C     !DASH
      save
C     !DASH
      real*8 A, CODSRW, ZERO
      integer I, IE, IM, IS, IZ, J, JM, K, KNT, KODSRW, NO
      logical JA
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  LINER, TATAR, NAUGHTD, HI, BYE
      intrinsic min
C
C               A(IM,JM), CODSRW(JM)
      dimension A(IM,*),  CODSRW(*)
C
      call HI ('DEZA')
C     !BEG
      if(NO.gt.0) then
        write (NO,100)
  100   format(' ','  The three indices "I(J)K" (below) have the ',
     $             'following meaning:'/
     $         ' ','  I  is the ordinal of the Ray.'/
     $         ' ','  J  is the index (= depth index) of the Shell ',
     $             'to which the i''th Ray is tangent.'/
     $         ' ','  K  is a code which tells how the WN-Matrix for ',
     $             'the i''th Ray is computed -'/
     $         ' ','     =0 means: it is computed directly;'/
     $         ' ','     =1 means: it is obtained by single-step ',
     $             'interpolation (MSKIP =1 or =3);'/
     $         ' ','     =2 means: it is obtained by double-step ',
     $             'interpolation (MSKIP =3).')
        call LINER (2, NO)
        write (NO,101)
  101   format(' ','Distances along Shell rays (cm).')
C     !EJECT
        IE = 0
  102   continue
          IS  = IE+1
          IE  = min(IE+9,IM)
          KNT = IE-IS+1
C
          call LINER     (2, NO)
          write (NO,103) (I,I=IS,IE)
  103     format(' ','   Indices',9I13)
          call LINER     (1, NO)
C
          K = 0
          do 106 J = 1,JM
            call TATAR   (K)
            call NAUGHTD (A(IS,J), 1, KNT, JA)
            if(.not.JA) then
              IZ = IE
  104         continue
                if(A(IZ,J).eq.ZERO) then
                  IZ = IZ-1
                  goto 104
                end if
              continue
              KODSRW = CODSRW(J)
              write (NO,105) J,K,KODSRW,(A(I,J),I=IS,IZ)
  105         format(' ',I3,'(',I3,')',I2,1P9E13.5)
            end if
  106     continue
C
        if(IE.lt.IM) goto 102
      end if
C     !END
      call BYE ('DEZA')
C
      return
      end
