      subroutine MERULA
     $(DEE)
C
C     Rudolf Loeser, 1990 Jul 02
C---- Dumps d-coefficients and related data, for diffusion calculation.
C     !DASH
      save
C     !DASH
      real*8 CD22, CD33, CD44, DEE, DIFAC, DTFAC, FZION, TEXP, XICA,
     $       XICB, XICC, XICD, ZXMIN
      integer I, IDFDM, J, LUEO, NEFDF
      character LAB*8
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
      equivalence (KZQ(111),IDFDM)
      equivalence (RZQ(112),ZXMIN)
      equivalence (RZQ(128),FZION)
      equivalence (KZQ(163),NEFDF)
C
C---- BAMBI       as of 1998 Apr 22
      integer     IPDPAR
      real*8      APDPAR
      dimension   APDPAR(10)
      common      /BAMBI1/ IPDPAR
      common      /BAMBI2/ APDPAR
C     Parameters for "original" d coefficients
C     .
      equivalence
     $(APDPAR( 1),TEXP  ),(APDPAR( 2),XICA  ),(APDPAR( 3),XICB  ),
     $(APDPAR( 4),XICC  ),(APDPAR( 5),DIFAC ),(APDPAR( 6),DTFAC ),
     $(APDPAR( 7),XICD  ),(APDPAR( 8),CD22  ),(APDPAR( 9),CD33  ),
     $(APDPAR(10),CD44  )
C     .
C---- ERODIUM     as of 2007 Apr 13
      integer     IPDFON
      real*8      APDFON,APDPPR
      dimension   APDFON(9), APDPPR(6)
      common      /ERODIUM1/ IPDFON
      common      /ERODIUM2/ APDFON,APDPPR
C     Parameters for "improved" d coefficients.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LINER, ARROUT, HI, BYE
C
C               DEE(4,5,N)
      dimension DEE(4,5,*)
C
      dimension LAB(2)
C
      data LAB /'inferred', ' actual '/
C
      call HI ('MERULA')
C     !BEG
      if((IDFDM.eq.0).or.(IDFDM.eq.1)) then
        if(IDFDM.eq.0) then
          J = IPDPAR
        else
          J = IPDFON
        end if
C
        call LINER  (2, LUEO)
        write (LUEO,100) J,ZXMIN,FZION
  100   format(' ','Calculation of d coefficients at depth #',I3,16X,
     $             'ZXMIN',1PE15.7,10X,'FZION',E15.7)
        call LINER  (1, LUEO)
C
        if(IDFDM.eq.0) then
          write (LUEO,101) TEXP,XICA,XICB,XICC,XICD,DIFAC,DTFAC,
     $                     CD22,CD33,CD44
  101     format(' ','TEXP ',1PE12.4/
     $           ' ','XICA ',E12.4,5X,'XICB ',E12.4,5X,'XICC ',E12.4,
     $               5X,'XICD ',E12.4/
     $           ' ','DIFAC',E12.4,5X,'DTFAC',E12.4/
     $           ' ','CD22 ',E12.4,5X,'CD33 ',E12.4,5X,'CD44 ',E12.4)
        else
          write (LUEO,102) (APDFON(I),I=1,6)
  102     format(' ',1PE14.6,' temperature, TEMP'/
     $           ' ',  E14.6,' total gas pressure, PGAS'/
     $           ' ',  E14.6,' BETA = (NHEK + NHE21) / 2'/
     $           ' ',  E14.6,' XIH = NHP / NH1 (= XION)'/
     $           ' ',  E14.6,' XIM = BETA / NHE1'/
     $           ' ',  E14.6,' XIP = NHE2K / BETA')
          write (LUEO,103) APDFON(7),LAB(NEFDF),APDFON(8),APDFON(9),
     $                   APDPPR
  103     format(' ',1PE14.6,' ',A,' electron density, ENE'/
     $           ' ',  E14.6,' Helium abundance, ABUN'/
     $           ' ',  E14.6,' Helium mass, AMAS'//
     $           ' ','Partial pressures:'//
     $           ' ',6X,'Hydrogen',7X,'Protons',11X,'HeI',10X,'HeII',
     $               9X,'HeIII',5X,'Electrons'/
     $           ' ', 6E14.6)
        end if
C
        call ARROUT (LUEO, DEE(1,1,J), 4, 5, 'Matrix of d coefficients')
      end if
C     !END
      call BYE ('MERULA')
C
      return
      end
