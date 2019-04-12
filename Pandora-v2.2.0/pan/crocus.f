      subroutine CROCUS
     $(NO,NQ,N,NT,YBAR,BDS,BDJ,BDQ,BDIJ,RHOIJ,RHOJ,RHOS,RHOW,WEIGHT,
     $ BDR,S,RBDQ,CHI,ASTAR,AW,SA,TRA,KTRAS,ABDR,ARHO)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Prints, for TULIP.
C     (This is version 2 of CROCUS.)
C     !DASH
      save
C     !DASH
      real*8 ABDR, ARHO, ASTAR, AW, BDIJ, BDJ, BDQ, BDR, BDS, CHI, RBDQ,
     $       RHOIJ, RHOJ, RHOS, RHOW, S, SA, TRA, WEIGHT, YBAR
      integer IL, IU, IUL, J, JBDNC, KLIN, KTRAS, N, NO, NQ, NT
C     !COM
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS( 3),KLIN )
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 40),JBDNC)
C     !DASH
      external INTRANS, SYRTA, RITA, VECOUT, SUCCOR, PET, HI, BYE
C
C               BDR(N,NL), BDS(N,NT), WEIGHT(N,NT), BDIJ(N,NL), TRA(N),
      dimension BDR(*),    BDS(N,*),  WEIGHT(N,*),  BDIJ(*),    TRA(*),
C
C               YBAR(N,NT), BDJ(N,NL), RBDQ(N,NL), RHOS(N,NT), S(N,NT),
     $          YBAR(N,*),  BDJ(*),    RBDQ(*),    RHOS(N,*),  S(N,*),
C
C               RHOJ(N,NT), ASTAR(N,NT), CHI(N,NT), AW(N,NT), SA(N,NT),
     $          RHOJ(N,*),  ASTAR(N,*),  CHI(N,*),  AW(N,*),  SA(N,*),
C
C               RHOW(N,NT), RHOIJ(N,NT), ABDR(N,3), ARHO(N,3),
     $          RHOW(N,*),  RHOIJ(N,*),  ABDR(*),   ARHO(*),
C
C               BDQ(N,NL)
     $          BDQ(*)
C     !EJECT
C
      call HI ('CROCUS')
C     !BEG
      if((NO.gt.0).or.(NQ.gt.0)) then
C
C----   Loop over all radiative transitions
        do 100 J = 1,NT
          call PET          (J)
          if(KLIN.eq.1) then
            call INTRANS    (IU, IL, 'CROCUS', IUL)
C
            if(NO.gt.0) then
              if(JBDNC.gt.0) then
C----           Print without analysis
                call SUCCOR (NO, N, IU, IL, BDIJ, RHOIJ(1,IUL),
     $                       WEIGHT(1,IUL), YBAR(1,IUL), S(1,IUL),
     $                       CHI(1,IUL))
              else
C----           Print with analysis
                call RITA   (NO, N, IU, IL, BDJ, BDR, BDQ, BDS(1,IUL),
     $                       RHOS(1,IUL), RHOJ(1,IUL), RHOW(1,IUL),
     $                       BDIJ, RHOIJ(1,IUL), WEIGHT(1,IUL),
     $                       CHI(1,IUL), S(1,IUL), YBAR(1,IUL), ABDR,
     $                       ARHO)
              end if
            end if
C
            if(NQ.gt.0) then
C----         Print details of BDQ calculation
              call SYRTA    (NQ, N, IU, IL, YBAR(1,IUL), RHOS(1,IUL),
     $                       S(1,IUL), AW(1,IUL), CHI(1,IUL),
     $                       SA(1,IUL), ASTAR(1,IUL), RBDQ)
            end if
          end if
  100   continue
C
        if(KTRAS.gt.1) then
C----     Print "Artificial TAU" for RHOW
          call VECOUT       (NO, TRA, N, 'Artificial TAU for RHOW')
        end if
C
      end if
C     !END
      call BYE ('CROCUS')
C
      return
      end
