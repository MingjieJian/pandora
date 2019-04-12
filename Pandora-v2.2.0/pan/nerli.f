      subroutine NERLI
     $(NO,N,NSL,TE,TR)
C
C     Rudolf Loeser, 1991 Jul 31
C---- Prints effective radiation temperatures
C     (outside the context of the Rates caclulations, under
C     separate option control.)
C     !DASH
      save
C     !DASH
      real*8 TE, TR
      integer I, IB, IE, J, N, NO, NSL
C     !DASH
      external  PRIAM, LINER, HI, BYE
      intrinsic min
C
C               TE(N), TR(N,NSL)
      dimension TE(*), TR(N,*)
C
      call HI ('NERLI')
C     !BEG
      call PRIAM   (NO,'TR(EFF)',7)
      call LINER   (2,NO)
      write (NO,100)
  100 format(' ','Effective Radiation Temperatures, for each level ',
     $           'at each depth.')
C
      IE = 0
  101 continue
        IB = IE+1
        IE = min((IE+8),N)
C
        call LINER (2,NO)
        write (NO,102) (I,I=IB,IE)
  102   format(' ',7X,8I15)
C
        call LINER (1,NO)
        write (NO,103) (TE(I),I=IB,IE)
  103   format(' ','TE',5X,1P8E15.7)
C
        do 105 J = 1,NSL
          write (NO,104) J,(TR(I,J),I=IB,IE)
  104     format(' ','TR(',I2,') ',1P8E15.7)
  105   continue
      if(IE.lt.N) goto 101
C     !END
      call BYE ('NERLI')
C
      return
      end
