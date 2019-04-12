      subroutine ALBERT
     $(NO,JM,XLMM,XMLC)
C
C     Rudolf Loeser, 1971 Nov 03
C---- Prints Opacity Multipliers.
C     !DASH
      save
C     !DASH
      real*8 ONE, XLMM, XMLC
      integer I, IB, IE, JM, NO
      logical OMULT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  KONSTD, LINER, HI, BYE
      intrinsic min
C
C               XLMM(JM), XMLC(JM)
      dimension XLMM(*),  XMLC(*)
C
      call HI ('ALBERT')
C     !BEG
      if((JM.gt.0).and.(NO.gt.0)) then
        call LINER     (2,NO)
        write (NO,100)
  100   format(' ','Opacity Multipliers')
C
        call KONSTD    (XMLC,1,JM,ONE,OMULT)
        if(.not.OMULT) then
          IE = 0
  101     continue
            IB = IE+1
            IE = min(IE+10,JM)
            call LINER (1,NO)
            write (NO,102) (XLMM(I),I=IB,IE)
  102       format(' ','LMM',4X,1P10E12.5)
            write (NO,103) (XMLC(I),I=IB,IE)
  103       format(' ','MLC',4X,10F12.3)
          if(IE.lt.JM) goto 101
C
        else
          call LINER   (1,NO)
          write (NO,104)
  104     format(' ','All values of MLC = 1.')
        end if
      end if
C     !END
      call BYE ('ALBERT')
C
      return
      end
