      subroutine GORNA
     $(WN,AN,N,TE,VM,H1,BD1,PL,UL,EMUX1,VL,XL)
C
C     Rudolf Loeser, 2003 Jan 08
C---- Computes intermediates, for ROWAN.
C     !DASH
      save
C     !DASH
      real*8 AN, ANGPCM, BD1, EIGHT, EMUX1, ENP, FAC, H1, PI, PL,
     $       ROOTPI, TE, UL, VL, VM, WN, WTM, XK1, XL, ZERO, dummy
      integer I, INR, K, N, NR
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 6),ANGPCM)
      equivalence (TUNI( 1),PI    )
      equivalence (TUNI( 2),ROOTPI)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 9),EIGHT )
C
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C     !DASH
C     !EJECT
      external KINAL, NARGO, HERRING, MINNOW, SARDINE, HI, BYE
C
C               WN(NLL), AN(NLL,N), PL(N), XL(N), UL(N), VL(N), BD1(N),
      dimension WN(*),   AN(NLL,*), PL(*), XL(*), UL(*), VL(*), BD1(*),
C
C               TE(N), VM(N), H1(N), EMUX1(N)
     $          TE(*), VM(*), H1(*), EMUX1(*)
C
      data NR /15/
C
      data FAC,XK1 /0.D0, 1.D0/
C
      call HI ('GORNA')
C     !BEG
      if(FAC.eq.ZERO) then
        INR = (NLL+1)-NR
        ENP = NR**5
        FAC = ENP/(EIGHT*PI*ROOTPI)
      end if
C
      WTM = (WN(INR)/ANGPCM)**3
C
      do 100 I = 1,N
        call KINAL (NR, TE(I), VM(I), dummy, XL(I))
C
        call NARGO (I, K)
        PL(I) = (AN(INR,K)*FAC*WTM*H1(I))/XL(I)
  100 continue
C
      call HERRING (N, TE, UL)
      call SARDINE (XK1, N, UL, EMUX1)
      call MINNOW  (N, EMUX1, BD1, VL)
C     !END
      call BYE ('GORNA')
C
      return
      end
