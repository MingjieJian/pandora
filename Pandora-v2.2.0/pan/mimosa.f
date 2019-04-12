      subroutine MIMOSA
     $(KK,XK,AK,GK,N,Z,TNU,V,CNXP,F1,XINK,FINK,EXT,RNDT,DNRT,DNRTC,
     $ CPK,KOOL,IDNRT)
C
C     Rudolf Loeser, 1979 Nov 30
C---- Computes the Incident Radiation Terms of the Lyman calculation,
C     for FERN.
C     (This is version 3 of MIMOSA.)
C     !DASH
      save
C     !DASH
      real*8 AK, CA, CB, CC, CNXP, CON33, CON53, CON7, CPK, DNRT, DNRTC,
     $       EXT, F1, FAC, FINK, GK, RNDT, TNU, TRM, V, XINK, XK, XKK,
     $       XNUK, Z
      integer I, IDNRT, K, KK, N
      logical KOOL
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
      equivalence (RZQ(  9),XNUK )
C     !DASH
C     !EJECT
      external ZERO1, RIGEL, STURM, ARRDIV, CONDIV, CONMUL, HI, BYE
C
C               XK(KKX), AK(KKX), GK(KKX), DNRTC(N), V(N,KKX), DNRT(N),
      dimension XK(*),   AK(*),   GK(*),   DNRTC(*), V(N,*),   DNRT(*),
C
C               CNXP(N,KKX), XINK(INK), FINK(INK), TNU(N,KKX), RNDT(N),
     $          CNXP(N,*),   XINK(*),   FINK(*),   TNU(*),     RNDT(*),
C
C               EXT(N), F1(N), Z(N)
     $          EXT(*), F1(*), Z(*)
C
      call HI ('MIMOSA')
C     !BEG
C---- Initialize
      call RIGEL      (7,  CON7 )
      call RIGEL      (33, CON33)
      call RIGEL      (53, CON53)
      CA = (XNUK**3)*CON7
      CB = CPK*CON33
      CC = CPK*CON53
      call ZERO1      (RNDT,  N)
      call ZERO1      (DNRT,  N)
      call ZERO1      (DNRTC, N)
C---- Compute CNXP
      call STURM      (N, KK, TNU, XK, XINK, FINK, Z, XNUK, EXT, CNXP)
C
C---- Compute sums over frequency
      do 101 I = 1,N
        do 100 K = 1,KK
          XKK = XK(K)
          FAC = (AK(K)*GK(K))/(XKK**3)
          TRM = FAC*CNXP(I,K)
C
          RNDT(I) = RNDT(I) + TRM*(V(I,K)/XKK)
          if(IDNRT.gt.0) then
            DNRT(I) = DNRT(I) + TRM/XKK
            if(KOOL) then
              DNRTC(I) = DNRTC(I) + TRM
            end if
          end if
  100   continue
  101 continue
C
C---- Final normalization
      call ARRDIV     (RNDT, F1, RNDT, N)
      call CONDIV     (CA, RNDT, N)
      if(IDNRT.gt.0) then
        call CONMUL   (CB, DNRT,  N)
        if(KOOL) then
          call CONMUL (CC, DNRTC, N)
        end if
      end if
C     !END
      call BYE ('MIMOSA')
C
      return
      end
