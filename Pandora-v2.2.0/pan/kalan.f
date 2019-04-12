      subroutine KALAN
     $(NO,N,FRS,S,XJBAR,AW,EP,BS,ICE,GMMA,XR,ANT,T,UNT,GMAI,DRLIMI,
     $ SN,SR,CHI)
C
C     Rudolf Loeser, 2004 May 07
C---- Supplementary printout for PSHAW.
C     (This is version 2 of KALAN.)
C     !DASH
      save
C     !DASH
      real*8 ANT, AW, BS, CHI, DRLIMI, EP, FRS, GMAI, GMMA, S, SN, SR,
     $       T, UNT, XJBAR, XR
      integer I, ICE, ISCMP, N, NO
      logical PDR, PGM, PSR
      character BLANK*1, CD*13, CS*13, CSN*13, QDR*11, QGM*11, QSR*11,
     $          SIG*1
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(178),ISCMP)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external ISOTRIA, LINER, NOTICE, SHIM, HI, BYE
C
C               XJBAR(N), DRLIMI(N), ANT(N), BS(N), SN(N), SR(N), T(N),
      dimension XJBAR(*), DRLIMI(*), ANT(*), BS(*), SN(*), SR(*), T(*),
C
C               UNT(N), AW(N), CHI(N), FRS(N), S(N), EP(N), GMAI(N)
     $          UNT(*), AW(*), CHI(*), FRS(*), S(*), EP(*), GMAI(*)
C     !EJECT
C
      call HI ('KALAN')
C     !BEG
      if((ISCMP.gt.0).and.(NO.gt.0)) then
        call ISOTRIA  (N, FRS, S, SR, PSR, XJBAR, AW, EP, BS, CHI, ICE,
     $                 GMMA, XR, PGM, PDR)
C
        call LINER    (3, NO)
        write (NO,100)
  100   format(' ',5X,'_____ Normalization  Terms _____',2X,
     $             'Spherical',18X,'---- PRD  Terms ----',2X,
     $             '===== Source Function Comparisons ====='/
     $         ' ',10X,'ANT',4X,'*',4X,'T',4X,'=',4X,'UNT',5X,
     $             'S * r**2',7X,'CHI',10X,'GAMMA', 5X,'DR-limit',7X,
     $             'S(n)',12X,'S',9X,'compared')
        call LINER    (1, NO)
C
        do 103 I = 1,N
          QSR = BLANK
          QGM = BLANK
          QDR = BLANK
          SIG = BLANK
  101     format(1PE11.3)
          if(PSR) then
            write (QSR,101) SR(I)
          end if
          if(PGM) then
            write (QGM,101) GMAI(I)
          end if
          if(PDR) then
            write (QDR,101) DRLIMI(I)
          end if
          call NOTICE (13, SN(I), S(I), CSN, CS, CD)
          write (NO,102) I, ANT(I), T(I), UNT(I), QSR, CHI(I), QGM,
     $                   QDR, CSN, CS, CD
  102     format(' ',I5,1P2E11.3,0PF10.6,A11,1PE15.6,1X,2A11,2X,3A13)
          call SHIM   (I, 5, NO)
  103   continue
C
        call LINER   (1, NO)
        write (NO, 104)
  104   format(' ',51X,'for CHI, see'/
     $         ' ',51X,'   Section  '/
     $         ' ',51X,'"RHO AND BD"'/
     $         ' ',51X,'    below   ')
      end if
C     !END
      call BYE ('KALAN')
C
      return
      end
