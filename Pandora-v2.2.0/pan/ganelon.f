      subroutine GANELON
     $(N,NL,NT,CRL,CRT,KHFF,RHFF,RHMFF,SUMH,KCRH,CRH,KCOND,CONX,KLNS,
     $ RLINS,KRAY,XRAY,KCOL,COL,SUM)
C
C     Rudolf Loeser, 1995 May 30
C---- Computes total cooling rate, and Hydrogen-only subtotal.
C     !DASH
      save
C     !DASH
      real*8 COL, CONX, CRH, CRL, CRT, RHFF, RHMFF, RLINS, SUM, SUMH,
     $       XRAY
      integer IL, IU, IUL, J, KCOL, KCOND, KCRH, KHFF, KLIN, KLNS, KRAY,
     $        KS, N, NL, NOION, NT
      logical HYDR
      character QELSM*8
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 94),NOION)
C     !DASH
      external  ZERO1, MOVE1, MERGINI, INTRANS, PET, ARRADD, HI, BYE
C
C               CRL(N,NL), CRT(N,NT), SUM(N), SUMH(N), RHFF(N), COL(N),
      dimension CRL(N,*),  CRT(N,*),  SUM(*), SUMH(*), RHFF(*), COL(*),
C
C               CRH(N), RHMFF(N), CONX(N), RLINS(N), XRAY(N)
     $          CRH(*), RHMFF(*), CONX(*), RLINS(*), XRAY(*)
C
      dimension KS(6)
C
      call HI ('GANELON')
C     !BEG
      HYDR = QELSM.eq.'H  '
      call MERGINI       (HYDR,KS)
C
      call ZERO1         (SUM,N)
C
      if(NOION.le.0) then
        do 100 J = 1,NL
          call ARRADD    (CRL(1,J),SUM,SUM,N)
  100   continue
        do 101 J = 1,NT
          call PET      (J)
          if(KLIN.eq.1) then
            call INTRANS (IU,IL,'GANELON',IUL)
            call ARRADD  (CRT(1,IUL),SUM,SUM,N)
          end if
  101   continue
      end if
C     !EJECT
      if((KHFF.eq.1).and.(KS(6).eq.1)) then
        call ARRADD (RHFF,SUM,SUM,N)
      end if
C
      if(HYDR) then
        call MOVE1  (SUM,N,SUMH)
      end if
C
      if((KCRH.eq.1).and.(KS(1).eq.1)) then
        call ARRADD (CRH,SUM,SUM,N)
      end if
      if((KHFF.eq.1).and.(KS(1).eq.1)) then
        call ARRADD (RHMFF,SUM,SUM,N)
      end if
      if((KCOND.eq.1).and.(KS(2).eq.1)) then
        call ARRADD (CONX,SUM,SUM,N)
      end if
      if((KLNS.gt.0).and.(KS(3).eq.1)) then
        call ARRADD (RLINS,SUM,SUM,N)
      end if
      if((KRAY.gt.0).and.(KS(4).eq.1)) then
        call ARRADD (XRAY,SUM,SUM,N)
      end if
      if((KCOL.gt.0).and.(KS(5).eq.1)) then
        call ARRADD (COL,SUM,SUM,N)
      end if
C     !END
      call BYE ('GANELON')
C
      return
      end
