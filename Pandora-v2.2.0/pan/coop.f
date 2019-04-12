      subroutine COOP
     $(W,IW,NEWCOL,XLM,N,NOPAC,KOPAC,ISWA,TE,V,VXS,HN,CON,CONT,
     $ OPAC,RSC)
C
C     Rudolf Loeser, 2004 Aug 31
C---- Computes CO-lines absorption and scattering.
C     (This is version 3 of COOP.)
C     !DASH
      save
C     !DASH
      real*8 CON, CONT, HN, OPAC, RSC, TE, V, VXS, W, XLM
      integer ISWA, IW, KOPAC, N, NOPAC
      logical DOIT, NEWCOL
C     !DASH
      external SLEUTH, RHODES, THERON, TIMARU, WARTS, HI, BYE
C
      dimension W(*), IW(*)
C
C               TE(N), V(N), VXS(N), CON(N), HN(N,Limp), CONT(Nopac,N),
      dimension TE(*), V(*), VXS(*), CON(*), HN(N,*),    CONT(NOPAC,*),
C
C               KOPAC(Nopac), ISWA(Nopac), OPAC(N), RSC(N)
     $          KOPAC(*),     ISWA(*),     OPAC(*), RSC(*)
C
      call HI ('COOP')
C     !BEG
      if((KOPAC(27).gt.0).or.(KOPAC(30).gt.0)) then
C----   Compute intermediates
        call SLEUTH (XLM, N, TE, V, VXS, HN, CON, OPAC)
        call RHODES (KOPAC(30), HN, N, RSC)
      end if
C
C---- CO-lines absorption
      call WARTS    (27, NEWCOL, KOPAC, CONT, NOPAC, N, DOIT)
      if(DOIT) then
        call THERON (27, NOPAC, N, OPAC, RSC, CONT)
        ISWA(27) = 1
      end if
C
C---- CO-lines scattering
      call WARTS    (30, NEWCOL, KOPAC, CONT, NOPAC, N, DOIT)
      if(DOIT) then
        call TIMARU (30, NOPAC, N, OPAC, RSC, CONT)
        ISWA(30) = 1
      end if
C     !END
      call BYE ('COOP')
C
      return
      end
