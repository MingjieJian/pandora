      subroutine LOCKET
     $(N,Z,TE,A,XNE,XNP,XNH1,XNHE11,XNHEP,XNHE2K,NS,ELL,EMAX,LU,S,E,
     $ EV,ELLH,ELLE,ELLHE2,ELLHE)
C
C     Rudolf Loeser, 1984 May 08
C---- Computes S and EV (and other auxiliaries), for PENDANT.
C     !DASH
      save
C     !DASH
      real*8 A, BOLZMN, E, ELL, ELLE, ELLH, ELLHE, ELLHE2, EMAX, ERGPEV,
     $       EV, S, TE, XNE, XNH1, XNHE11, XNHE2K, XNHEP, XNP, Z
      integer I, IQEDD, LU, N, NS
      logical DUMP
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(176),IQEDD)
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 2),BOLZMN)
      equivalence (PCON(13),ERGPEV)
C     !DASH
      external ZERO1, ESS, DRUMLIN, ELDER, MESHED, MASHED, HI, BYE
C
C               EV(N), XNE(N), E(N), S(N), ELLH(N), ELLE(N), ELLHE2(N),
      dimension EV(*), XNE(*), E(*), S(*), ELLH(*), ELLE(*), ELLHE2(*),
C
C               ELLHE(N), Z(N), A(N), TE(N), XNP(N), XNHEP(N), XNH1(N),
     $          ELLHE(*), Z(*), A(*), TE(*), XNP(*), XNHEP(*), XNH1(*),
C
C               XNHE11(N), XNHE2K(N)
     $          XNHE11(*), XNHE2K(*)
C     !EJECT
C
      call HI ('LOCKET')
C     !BEG
C---- Initialize
      call ZERO1       (S     , N)
      call ZERO1       (E     , N)
      call ZERO1       (EV    , N)
      call ZERO1       (ELLH  , N)
      call ZERO1       (ELLE  , N)
      call ZERO1       (ELLHE , N)
      call ZERO1       (ELLHE2, N)
C
      if(NS.lt.N) then
        DUMP = (LU.gt.0).and.(IQEDD.gt.0)
        if(DUMP) then
          call MESHED  ('LOCKET', 2)
          call DRUMLIN (N, Z, A, XNE, XNP, XNH1, XNHE11, XNHEP, XNHE2K)
        end if
C
C----   Starting values
        E(NS)  = BOLZMN*TE(NS)
        EV(NS) = E(NS)/ERGPEV
C
        call ESS       (NS, E, S, ELL, A, ELLE, ELLH, ELLHE, ELLHE2,
     $                  XNE, XNP, XNH1, XNHE11, XNHEP, XNHE2K, DUMP)
C
C----   Loop over all points to be computed
        do 100 I = (NS+1),N
          call ELDER   (I, E, S, ELL, Z, A, XNE, XNP, XNH1, XNHE11,
     $                  XNHEP, XNHE2K, ELLH, ELLE, ELLHE2, ELLHE, DUMP)
C
          EV(I) = E(I)/ERGPEV
          if(E(I).gt.EMAX) then
            goto 101
          end if
  100   continue
        if(DUMP) then
          call MASHED  ('LOCKET')
        end if
C
      end if
  101 continue
C     !END
      call BYE ('LOCKET')
C
      return
      end
