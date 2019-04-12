      subroutine OSTUNI
     $(MIST,LIST,L,DIDH,MYX,MUX,YY,XLTIT,NW,IU,IL,EMU,RUN,Z,TE,N,NO,
     $ WTAB,KODE,DETAIL)
C
C     Rudolf Loeser, 1991 Aug 08
C---- Produces dI/dh output.
C     !DASH
      save
C     !DASH
      real*8 DIDH, DMAX, EMU, RUN, TE, WTAB, XLTIT, YY, Z
      integer I, ICDIT, IL, IU, J, KODE, L, LIST, M, MIST, MUX, MYX, N,
     $        NO, NW
      logical DETAIL, DOPLOT, PLOTOK, RUNOK
      character BLANK*1, PLOT*1
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
      equivalence (KZQ(120),ICDIT)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external  OBALL, EYLAU, OSSAR, OLARBAR, OINOE, OLIDES, FRUNZE,
     $          MOVEI, HALT, HI, BYE
      intrinsic abs
C
C               LL = KM (KODE = 1), or Nmkuse (KODE = 2)
C
C               DIDH(N,LL), MUX(LL), MYX(LL), YY(LL), WTAB(LL), RUN(N),
      dimension DIDH(N,*),  MUX(*),  MYX(*),  YY(*),  WTAB(*),  RUN(*),
C
C               XLTIT(LL), LIST(LL), MIST(LL), TE(N), Z(N)
     $          XLTIT(*),  LIST(*),  MIST(*),  TE(*), Z(*)
C
      call HI ('OSTUNI')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.2)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1 or 2.')
        call HALT   ('OSTUNI', 1)
      end if
C
C---- Write header
      call OBALL    (NO, KODE, IU, IL, ICDIT, DETAIL, Z, TE, N)
C---- Make working copy of master list
      call MOVEI    (MIST, 1, L, LIST, 1, L)
C
      if(DETAIL) then
C----   Mark selected runs for plotting
        call EYLAU  (L, LIST, MYX)
C----   Initialize plot
        call OSSAR  (IMAGE, DIDH, MYX, NW, LIST, L, Z, N, PLOTOK)
      else
        PLOTOK = .false.
      end if
C     !EJECT
      M = 0
      do 101 I = 1,L
        J = abs(LIST(I))
C
        DOPLOT = (LIST(I).lt.0).and.PLOTOK
        if(DOPLOT) then
          M = M+1
          PLOT = ALPHS(M)
        else
          PLOT = BLANK
        end if
C
C----   Get MAX and RUN
        call OLARBAR (DIDH(1,J), MYX(J), DMAX, RUN, N, RUNOK)
C----   Print this RUN
        call OLIDES  (NO, KODE, WTAB(J), EMU, YY(J), MUX(J), MYX(J),
     $                DMAX, PLOT, XLTIT(J), RUN, N, RUNOK)
        if(DOPLOT.and.RUNOK) then
          call OINOE (IMAGE, Z, DIDH(1,J), RUN, N, PLOT)
        end if
  101 continue
C
      if(PLOTOK) then
C----   Print plot
        call FRUNZE  (NO, IMAGE)
      end if
C     !END
      call BYE ('OSTUNI')
C
      return
      end
