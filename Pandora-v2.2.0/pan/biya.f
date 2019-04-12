      subroutine BIYA
     $(NO,N,K,IU,IL,DL,H1,H1M,Z,TE,RF,IQSHF,IQENH,ICE)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Prints, for ALDAN.
C     (This is version 2 of BIYA.)
C     !DASH
      save
C     !DASH
      real*8 DL, H1, H1M, RF, TE, Z
      integer I, ICE, IL, IQENH, IQSHF, IU, K, N, NO
      logical ENHAN, PRD, SNUSH
      character BLANK*1, LEFT*52, RITE*52
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  ABJECT, LINER, SHIM, HI, BYE
      intrinsic max
C
C               DL(K), H1(K), H1M(K), Z(N), TE(N), RF(N)
      dimension DL(*), H1(*), H1M(*), Z(*), TE(*), RF(*)
C
      call HI ('BIYA')
C     !BEG
      if(NO.gt.0) then
        call ABJECT (NO)
        write (NO,100) IU,IL
  100   format(' ','Flux Distribution at outer surface, H1, and ',
     $             'Radiative Force, RF, for transition (',I2,'/',I2,
     $             ').'//
     $         ' ',18X,'DL',14X,'H1',9X,'4*PI*H1',24X,'Z',14X,'TE',14X,
     $             'RF')
        call LINER  (1,NO)
C     !EJECT
        do 103 I = 1,(max(K,N))
          LEFT = BLANK
          if(I.le.K) then
            write (LEFT,101) I,DL(I),H1(I),H1M(I)
  101       format(I4,1P3E16.8)
          end if
C
          RITE = BLANK
          if(I.le.N) then
            write (RITE,101) I,Z(I),TE(I),RF(I)
          end if
C
          write (NO,102) LEFT,RITE
  102     format(' ',A52,5X,A52)
          call SHIM  (I,5,NO)
  103   continue
C
        SNUSH = IQSHF.gt.0
        ENHAN = IQENH.gt.0
        PRD = ICE.ne.0
        if(SNUSH.or.ENHAN.or.PRD) then
          call LINER (2,NO)
          write (NO,104)
  104     format(' ','The final Flux Profile calculation will differ ',
     $               'from this one because:')
          if(ENHAN) then
            write (NO,105)
  105       format(' ','The ENHANCE Option is on.')
          end if
          if(PRD) then
            write (NO,106)
  106       format(' ','Another PRD Snu calculation will intervene.')
          end if
          if(SNUSH) then
            write (NO,107)
  107       format(' ','The SNUSHFT Option is on.')
          end if
        end if
C
      end if
C     !END
      call BYE ('BIYA')
C
      return
      end
