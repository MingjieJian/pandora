      subroutine CINDER
     $(N,LU,INDEX,INAME,LEV,ESG,TE,PF,H1,HK,HE1,HEK,ALFH,ALFHE,ALL,
     $ CFH,CFHE,RKI,DORK,RLI,DORL)
C
C     Rudolf Loeser, 2004 Sep 20
C---- Does the lower-levl charge exchange calculation for level LEV.
C     !DASH
      save
C     !DASH
      real*8 ALFH, ALFHE, ALL, CFH, CFHE, CXH, CXHE, ESG, FAC, H1, HE1,
     $       HEK, HK, PF, RKAD, RKI, RLAD, RLI, TE
      integer I, INDEX, LEV, LU, N
      logical DORK, DORL, ISO1, PRNT
      character INAME*3
C     !DASH
      external ROBBER, ROTTER, ROCKER, ROLLER, PRANK, SPARK, ASHES,
     $         ZERO1, HI, BYE
C
C               TE(N), PF(N), ESG(N), HE1(N), HK(N), ALFH(N), ALFHE(N),
      dimension TE(*), PF(*), ESG(*), HE1(*), HK(*), ALFH(*), ALFHE(*),
C
C               CFH(N), CFHE(N), RKI(N), RLI(N), H1(N), HEK(N), ALL(N)
     $          CFH(*), CFHE(*), RKI(*), RLI(*), H1(*), HEK(*), ALL(*)
C
      data FAC /1.D-9/
C
      call HI ('CINDER')
C     !BEG
      call ROCKER    (INDEX, N, TE, ALFH )
      call ROLLER    (INDEX, N, TE, ALFHE)
      call ROBBER    (INDEX, N, TE, PF, CFH )
      call ROTTER    (INDEX, N, TE, PF, CFHE)
C
      ISO1 = INDEX.eq.9
      if(ISO1) then
        call PRANK   (LEV, N, TE, ALL)
      else
        call ZERO1   (ALL, N)
      end if
C
      call SPARK     (N, LU, INAME, LEV, INDEX, ISO1, RKI, RLI, TE, PF,
     $                ESG, CFH, CFHE, ALL, PRNT)
C     !EJECT
      do 100 I = 1,N
        CXH  = CFH(I)*ALFH(I)
        CXHE = CFHE(I)*ALFHE(I)
C
        RKAD = FAC*(HK(I)*CXH+HEK(I)*CXHE)
C
        if(ISO1) then
          RLAD = FAC*((H1(I)*ALL(I)*ALFH(I))/ESG(I))
        else
          RLAD = FAC*((H1(I)*ALFH(I)+HE1(I)*ALFHE(I))/ESG(I))
        end if
C
        if(DORK) then
          RKI(I) = RKI(I)+RKAD
        end if
C
        if(DORL) then
          RLI(I) = RLI(I)+RLAD
        end if
C
        if(PRNT) then
          call ASHES (I, N, LU, LEV, ALFH(I), ALFHE(I), CXH, CXHE,
     $                RKAD, RLAD, RKI(I), RLI(I))
        end if
  100 continue
C     !END
      call BYE ('CINDER')
C
      return
      end
