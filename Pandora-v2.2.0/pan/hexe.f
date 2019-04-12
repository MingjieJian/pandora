      subroutine HEXE
     $(K,N,XK,IADRS,XLM,FMULT,GK,ISLEV,XKKA,XKKB,TNU,SP,XLB,XJNU,
     $ SIGMA,CAPPAR,T1,TR,BHSR)
C
C     Rudolf Loeser, 1979 Nov 30
C---- Prints, for TEUFEL
C     !DASH
      save
C     !DASH
      real*8 BHSR, CAPPAR, FMULT, GK, SIGMA, SP, T1, TNU, TR, XJNU, XK,
     $       XKKA, XKKB, XLB, XLM
      integer I, IADRS, ISLEV, K, KLDIN, KLFIN, LUEO, N
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
      equivalence (KZQ(219),KLDIN)
      equivalence (KZQ(220),KLFIN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  LINER, HI, BYE
      intrinsic mod
C
C               XKKA(N), XKKB(N), CAPPAR(N), XLB(N), XJNU(N), SIGMA(N),
      dimension XKKA(*), XKKB(*), CAPPAR(*), XLB(*), XJNU(*), SIGMA(*),
C
C               TNU(N), T1(N), TR(N), BHSR(N), SP(N)
     $          TNU(*), T1(*), TR(*), BHSR(*), SP(*)
C
      call HI ('HEXE')
C     !BEG
      if(mod(K,KLFIN).eq.1) then
C
        if(K.gt.1) then
          call LINER (3, LUEO)
        end if
C
        write (LUEO,100) K,XK,XLM,FMULT,GK,IADRS,ISLEV
  100   format(' ','Wavelength',I5,F10.4,1PE20.10,20X,'Multiplier',
     $             0PF10.4,10X,'GK',F10.4,3X,'iadrs',I10/
     $         ' ',115X,'Absorption'/
     $         ' ',94X,'Reserved Absorption',4X,'Source'/
     $         ' ',84X,'Pure',5X,4('-----'),3X,'Function'/
     $         ' ',81X,'Absorption',25X,'without'/
     $         ' ',73X,'Pure',6X,'without',15X,'other',
     $             6X,'reserved'/
     $         ' ',70X,'Scattering',5X,'Ion',5X,'Level',I3,
     $             4X,'Levels',6X,'Level'//
     $         ' ',7X,'KKA',8X,'KKB',8X,'TNU',9X,'SP',9X,'LB',
     $             9X,'J',8X,'SIGMA',6X,'CAPPAR',7X,'T1',9X,'TR',
     $             8X,'BHSR')
        call LINER   (1, LUEO)
C
        write (LUEO,101) (I,XKKA(I),XKKB(I),TNU(I),SP(I),XLB(I),
     $                    XJNU(I),SIGMA(I),CAPPAR(I),T1(I),TR(I),
     $                    BHSR(I),I=1,N,KLDIN)
  101   format(5(' ',I3,1P11E11.3/))
C
      end if
C     !END
      call BYE ('HEXE')
C
      return
      end
