      subroutine FORMOSA
     $(PBL,N,ILFLX)
C
C     Rudolf Loeser, 1981 Dec 08
C---- Produces a Diana Data Blocks dump.
C     !DASH
      save
C     !DASH
      real*8 PBL
      integer I, ICE, ILFLX, IPR01, IPR02, LPALL, LPBC, LPJNU, LPKPC,
     $        LPNAM, LPPHI, LPPHW, LPSIG, LPT1, LPT2, LPT3, LPTNU, LPWH,
     $        LPWN, LPXLL, N
      logical KILROY
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
      equivalence (KZQ( 57),IPR01)
      equivalence (KZQ( 58),IPR02)
C
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
C     .
      equivalence
     $(LPD( 1),LPNAM),(LPD( 2),LPPHI),(LPD( 3),LPBC ),(LPD( 4),LPJNU),
     $(LPD( 5),LPT1 ),(LPD( 6),LPSIG),(LPD( 7),LPWN ),(LPD( 8),LPKPC),
     $(LPD( 9),LPPHW),(LPD(10),LPALL),(LPD(11),LPXLL),(LPD(12),LPWH ),
     $(LPD(13),LPTNU),(LPD(14),LPT2 ),(LPD(15),LPT3 )
C     .
C     !EJECT
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
      equivalence (LINKDS( 4),ICE  )
C     !DASH
C     !EJECT
      external LEYTE, ARUNTA, MUSHED, MASHED, HI, BYE
C
C               PBL(LPDLEN)
      dimension PBL(*)
C
      call HI ('FORMOSA')
C     !BEG
      if(NBOP.gt.0) then
C
        KILROY = .true.
        do 100 I = 1,NBOP
C
          if((I.ge.IPR01).and.(I.le.IPR02)) then
            call LEYTE  (PBL, LPDLEN, INDOP(I))
            call MUSHED ('FORMOSA', 2, KILROY)
            call ARUNTA (I, KEROP(I), ICE, ILFLX, N,
     $                   PBL(LPPHI), PBL(LPBC ), PBL(LPTNU),
     $                   PBL(LPJNU), PBL(LPSIG), PBL(LPT1 ),
     $                   PBL(LPT2 ), PBL(LPT3 ), PBL(LPWN ),
     $                   PBL(LPPHW), PBL(LPKPC), PBL(LPWH ),
     $                   PBL(LPNAM), PBL(LPALL), PBL(LPXLL))
          end if
C
  100   continue
C
        if(.not.KILROY) then
          call MASHED   ('FORMOSA')
        end if
C
      end if
C     !END
      call BYE ('FORMOSA')
C
      return
      end
