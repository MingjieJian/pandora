      subroutine SPICY
     $(NO,N,ITMX,INP,A)
C
C     Rudolf Loeser, 1978 Jul 27
C---- Prints iteration summaries.
C     INP = 1 means: input set included;
C         = 0 means: input set not included.
C     (This is version 2 of SPICY.)
C     !DASH
      save
C     !DASH
      real*8 A, ONE, VAL
      integer I, ID, IE, INP, IS, ITER, ITMX, KNT, N, NO
      character BLANK*1, LAB*5
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  LINER, PIXY, HI, BYE
      intrinsic min
C
C               A(N,ITMX)
      dimension A(N,*)
C
      dimension VAL(12)
C     !EJECT
C
      call HI ('SPICY')
C     !BEG
      if(NO.gt.0) then
C
C----   Loop over depths
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+12,N)
C
C----     Write depths header
          call LINER   (2,NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',2X,'Depth',12I10)
C
          if(INP.eq.1) then
C----       Print input values
            call PIXY  (A(1,1),IS,IE,VAL,KNT)
            call LINER (1,NO)
            LAB = 'Input'
            ID  = 0
            write (NO,102) LAB,ID,(VAL(I),I=1,KNT)
          end if
C
C----     Loop over iterations
          call LINER   (1,NO)
          LAB = 'Iter '
          do 103 ITER = (INP+1),(ITMX-1)
            call PIXY  (A(1,ITER),IS,IE,VAL,KNT)
            ID = ITER-INP
            write (NO,102) LAB,ID,(VAL(I),I=1,KNT)
  102       format(' ',A5,I2,12F10.4)
            LAB = BLANK
  103     continue
C
C----     Final values
          ID = ITMX-INP
          write (NO,102) LAB,ID,(ONE,I=IS,IE)
          call LINER   (1,NO)
          write (NO,104) (A(I,ITER),I=IS,IE)
  104     format(' ',2X,'Final',1P12E10.2)
C
        if(IE.lt.N) goto 100
C
      end if
C     !END
      call BYE ('SPICY')
C
      return
      end
