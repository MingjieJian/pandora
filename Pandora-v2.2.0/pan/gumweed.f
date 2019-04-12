      subroutine GUMWEED
     $(W,IW,NEWLYM,XLM,N,NOPAC,KOPAC,ISWA,ISWE,TE,HN,BDH,CO,CB,XLYB)
C
C     Rudolf Loeser, 2003 Oct 07
C---- Supervises calculation of H Lyman lines background emission.
C     (This is version 3 of GUMWEED.)
C     !DASH
      save
C     !DASH
      real*8 BDH, CB, CO, HN, TE, W, XLM, XLYB
      integer ILYP, ILYUL, ISWA, ISWE, IW, KOPAC, N, NOPAC
      logical DOIT, NEWDAT, NEWLYM
C     !COM
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
      equivalence (ILB(12),ILYP )
      equivalence (ILB(14),ILYUL)
C     !DASH
C     !EJECT
      external ANGELA, KELLA, SAIGA, WARTS, HI, BYE
C
      dimension W(*), IW(*)
C
C               CB(Nopac,N), CO(Nopac,N), HN(N,Limdat(1)), ISWE(Nopac),
      dimension CB(*),       CO(*),       HN(*),           ISWE(*),
C
C               TE(N), XLYB(Lenlyb), KOPAC(Nopac), BDH(N,Limdat(1)),
     $          TE(*), XLYB(*),      KOPAC(*),     BDH(*),
C
C               ISWA(Nopac)
     $          ISWA(*)
C
      call HI ('GUMWEED')
C     !BEG
C---- Lyman alpha emission
      NEWDAT = (ISWA(11).gt.0).or.NEWLYM
      call WARTS    (11, NEWDAT, KOPAC, CB, NOPAC, N, DOIT)
      if(DOIT) then
        call ANGELA (11, XLM, N, NOPAC, TE, HN, CO, CB)
        ISWE(11) = 1
      end if
C
C---- Higher (N = 3, ... 15) Lyman lines emission
      NEWDAT = (ISWA(34).gt.0).or.NEWLYM
      call WARTS    (34, NEWDAT, KOPAC, CB, NOPAC, N, DOIT)
      if(DOIT) then
        call KELLA  (34, XLM, N, NOPAC, TE, HN, XLYB(ILYP), CB)
        ISWE(34) = 1
      end if
C
C---- Highest (N > 15; limit = NLL) Lyman lines emission
      NEWDAT = (ISWA(37).gt.0).or.NEWLYM
      call WARTS    (37, NEWDAT, KOPAC, CB, NOPAC, N, DOIT)
      if(DOIT) then
        call SAIGA  (37, XLM, N, NOPAC, TE, HN, BDH, XLYB(ILYUL),
     $               CO, CB)
        ISWE(37) = 1
      end if
C     !END
      call BYE ('GUMWEED')
C
      return
      end
