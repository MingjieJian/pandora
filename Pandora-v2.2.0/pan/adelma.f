      subroutine ADELMA
     $(XLCOA,XLCOB,KODE,WVB,J,K,M,NB,IW)
C
C     Rudolf Loeser, 1987 Nov 16
C---- Adds the wavelengths in the current CO spectral band to the
C     basic CO wavelengths table.
C     (This is version 4 of ADELMA.)
C     !DASH
      save
C     !DASH
      real*8 WVB, XLCOA, XLCOB
      integer I, IPEX, IPNT, ISO, IVEC, IW, IWS, J, JN, K, KODE, L,
     $        LUEO, M, MUX, NB
      logical DUMP
C     !COM
C---- LOOM        as of 1994 Dec 16
      integer     JCOLMF,KCOLMF,JCOLMO,KCOLMO,JCOLMS,KCOLMS,
     $            JCOLMR,KCOLMR,JCOMAX,KCOMAX
      parameter   (JCOLMF=111, KCOLMF=20)
      parameter   (JCOLMO=111, KCOLMO=13)
      parameter   (JCOLMS=111, KCOLMS=12)
      parameter   (JCOLMR=53,  KCOLMR=21)
      parameter   (JCOMAX=max(JCOLMF,JCOLMO,JCOLMS,JCOLMR))
      parameter   (KCOMAX=max(KCOLMF,KCOLMO,KCOLMS,KCOLMR))
C     (Be sure to recompile all users of LOOM when changing any
C     of the above parameter values!)
      real*8      WNUPF,WNDNF,WNUPO,WNDNO,WNUPS,WNDNS,WNROT,
     $            FUPF ,FDNF ,FUPO ,FDNO ,FUPS ,FDNS ,FROT ,
     $            EXEN,WVCOLO,WVCOHI,RC1213
      integer     JCOFUN,KCOFUN,JCOOVR,KCOOVR,JCOSEC,KCOSEC,
     $            JCOROT,KCOROT,ISOSLCT,METHCOW,METHCOF
      logical     FUNDCO,OVERCO,SECNCO,ROTACO
C
      dimension   WNUPF(JCOLMF,KCOLMF,2), WNDNF(JCOLMF,KCOLMF,2),
     $            WNUPO(JCOLMO,KCOLMO,2), WNDNO(JCOLMO,KCOLMO,2),
     $            WNUPS(JCOLMS,KCOLMS,2), WNDNS(JCOLMS,KCOLMS,2),
     $            WNROT(JCOLMR,KCOLMR,2),
     $            FUPF(JCOLMF,KCOLMF,2),  FDNF(JCOLMF,KCOLMF,2),
     $            FUPO(JCOLMO,KCOLMO,2),  FDNO(JCOLMO,KCOLMO,2),
     $            FUPS(JCOLMS,KCOLMS,2),  FDNS(JCOLMS,KCOLMS,2),
     $            FROT(JCOLMR,KCOLMR,2),
     $            EXEN(JCOMAX+1,KCOMAX,2)
      dimension   FUNDCO(2),OVERCO(2),SECNCO(2),ROTACO(2),
     $            JCOFUN(2),KCOFUN(2),JCOOVR(2),KCOOVR(2),
     $            JCOSEC(2),KCOSEC(2),JCOROT(2),KCOROT(2)
C
C     Carbon Monoxide lines:  wavenumbers and oscillator strengths.
C
C     WNROT, etc., are for rotational lines     : j'-j=+1, v'-v=0;
C     WNUPF, etc., are for fundamental lines    : j'-j=+1, v'-v=1;
C     WNDNF, etc., are for fundamental lines    : j'-j=-1, v'-v=1;
C     WNUPO, etc., are for first overtone lines : j'-j=+1, v'-v=2;
C     WNDNO, etc., are for first overtone lines : j'-j=-1, v'-v=2;
C     WNUPS, etc., are for second overtone lines: j'-j=+1, v'-v=3;
C     WNDNS, etc., are for second overtone lines: j'-j=-1, v'-v=3.
C
C     This is how the array indices relate to the quantum numbers:
C
C     1. index J corresponds to quantum number j -
C                in WNROT, etc., j = J-1, j' = J
C                in WNUPF, etc., j = J-1, j' = J
C                in WNDNF, etc., j = J  , j' = J-1
C                in WNUPO, etc., j = J-1, j' = J
C                in WNDNO, etc., j = J  , j' = J-1
C                in WNUPS, etc., j = J-1, j' = J
C                in WNDNS, etc., j = J  , j' = J-1
C
C     2. index K corresponds to quantum number v (=nu) -
C                in WNROT, etc.,                 v = K-1, v' = K-1
C                in WNUPF, etc. and WNDNF, etc., v = K-1, v' = K
C                in WNUPO, etc. and WNDNO, etc., v = K-1, v' = K+1
C                in WNUPS, etc. and WNDNS, etc., v = K-1, v' = K+2
C
C     3. index L identifies the Carbon isotope -
C                L=1 for Carbon-12, L=2 for Carbon-13.
C
C     Wavenumbers and energies from:
C        Farrenq, R., Guelachvili, G., Sauval, A.J., Grevesse, N.,
C        and Farmer, C.B. 1991,
C        J.Molec.Spectrosc., 149, 375
C       or
C        Coxon, J.A., and Hajigeorgiou, P.G. 1992,
C        Can.J.Phys., 70, 40-54
C       or
C        Goorvitch, 1994, Ap.J.Suppl.
C     f-values from:
C        Chackerian, C., Jr, and Tipping, R. H. 1983
C        J.Molec.Spectros., 99, 431
C       or
C        "improved" Chackerian
C       or
C        Goorvitch, 1994, Ap.J.Suppl.
C
      common      /LOOM1 / EXEN
      common      /LOOM2 / WNROT,FROT
      common      /LOOM3 / WNUPF,FUPF,WNDNF,FDNF
      common      /LOOM4 / WNUPO,FUPO,WNDNO,FDNO
      common      /LOOM5 / WNUPS,FUPS,WNDNS,FDNS
      common      /LOOM6 / JCOFUN,KCOFUN,JCOOVR,KCOOVR,JCOSEC,KCOSEC,
     $                     JCOROT,KCOROT
      common      /LOOM7 / WVCOLO,WVCOHI
      common      /LOOM8 / FUNDCO,OVERCO,SECNCO,ROTACO
C
C     (These parameters are computed by subroutines WEAVE and FOCA.)
C
      common      /LOOM9 / RC1213
      common      /LOOM10/ ISOSLCT,METHCOW,METHCOF
C
C     RC1213  is the C(12)/C(13) isotopic abundance ratio.
C     ISOSLCT controls which isotopes are included in this calculation:
C     ISOSLCT = 1 means: C(12) only;
C     ISOSLCT = 2 means: C(13) only; and
C     ISOSLCT = 3 means: both C(12) and C(13).
C
C     METHCOF = 1 means: use f-values from Chackerian & Tipping (1983);
C     METHCOF = 2 means: use f-values from "new" Chackerian data;
C     METHCOF = 3 means: use f-values from Goorvitch (1994).
C
C     METHCOW = 1 means: use energies from Farrenq et al.;
C     METHCOW = 2 means: use energies from Coxon & Hajigeorgiou;
C     METHCOW = 3 means: use wave numbers & energies from
C                                                    Goorvitch (1994).
C     .
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external KALEM, SORT, ORDERI, ANYULA, MESHED, IGIVE, MASHED,
     $         HI, BYE
C
      dimension IW(*)
C
C               WVB(NB), J(NB), K(NB), M(NB)
      dimension WVB(*),  J(*),  K(*),  M(*)
C
      dimension JN(2)
      equivalence
     $(JN( 1),IPNT  ),(JN( 2),IVEC  )
C
      call HI ('ADELMA')
C     !BEG
C     (Get, and allocate, IW allotment)
      call KALEM    (JN, IWS, MUX, 'ADELMA')
C
      DUMP = (IPEX.lt.0).or.(IPEX.eq.1)
      if(DUMP) then
        call MESHED ('ADELMA', 2)
      end if
C
      NB = NB+1
      WVB(NB) = XLCOA
      if(KODE.eq.1) then
        J(NB) = 0
        K(NB) = 0
        M(NB) = 21
      end if
C     !EJECT
      do 108 ISO = 1,2
        if(ROTACO(ISO)) then
C----     Rotational lines
          do 101 I = 1,JCOROT(ISO)
            do 100 L = 1,KCOROT(ISO)
              call ANYULA (I, L, WNROT(I,L,ISO), XLCOA, XLCOB, (+KODE),
     $                     22, ISO, NB, WVB, J, K, M, DUMP)
  100       continue
  101     continue
        end if
        if(FUNDCO(ISO)) then
C----     Fundamental lines
          do 103 I = 1,JCOFUN(ISO)
            do 102 L = 1,KCOFUN(ISO)
              call ANYULA (I, L, WNUPF(I,L,ISO), XLCOA, XLCOB, (+KODE),
     $                     17, ISO, NB, WVB, J, K, M, DUMP)
              call ANYULA (I, L, WNDNF(I,L,ISO), XLCOA, XLCOB, (-KODE),
     $                     17, ISO, NB, WVB, J, K, M, DUMP)
  102       continue
  103     continue
        end if
        if(OVERCO(ISO)) then
C----     First overtone lines
          do 105 I = 1,JCOOVR(ISO)
            do 104 L = 1,KCOOVR(ISO)
              call ANYULA (I, L, WNUPO(I,L,ISO), XLCOA, XLCOB, (+KODE),
     $                     20, ISO, NB, WVB, J, K, M, DUMP)
              call ANYULA (I, L, WNDNO(I,L,ISO), XLCOA, XLCOB, (-KODE),
     $                     20, ISO, NB, WVB, J, K, M, DUMP)
  104       continue
  105     continue
        end if
        if(SECNCO(ISO)) then
C----     Second overtone lines
          do 107 I = 1,JCOSEC(ISO)
            do 106 L = 1,KCOSEC(ISO)
              call ANYULA (I, L, WNUPS(I,L,ISO), XLCOA, XLCOB, (+KODE),
     $                     23, ISO, NB, WVB, J, K, M, DUMP)
              call ANYULA (I, L, WNDNS(I,L,ISO), XLCOA, XLCOB, (-KODE),
     $                     23, ISO, NB, WVB, J, K, M, DUMP)
  106       continue
  107     continue
        end if
  108 continue
C     !EJECT
      NB = NB+1
      WVB(NB) = XLCOB
      if(KODE.eq.1) then
        J(NB) = 0
        K(NB) = 0
        M(NB) = 21
      end if
C
      call SORT     (WVB, NB, IW(IPNT), 'CO wavelengths')
      if(KODE.eq.1) then
        call ORDERI (J, IW(IPNT), NB, IW(IVEC))
        call ORDERI (K, IW(IPNT), NB, IW(IVEC))
        call ORDERI (M, IW(IPNT), NB, IW(IVEC))
      end if
C
      if(DUMP) then
        write (LUEO,109) KODE,ROTACO,FUNDCO,OVERCO,SECNCO
  109   format(' ','Constructing the CO wavelengths table.',20X,
     $             'KODE =',I10//
     $         ' ',4X,'ROTA',5X,'FUND',5X,'OVER',5X,'SECN'/
     $         ' ',4(L6,L3))
        do 111 I = 1,NB
          if(KODE.eq.1) then
            write (LUEO,110) I,WVB(I),J(I),K(I),M(I)
          else
            write (LUEO,110) I,WVB(I)
          end if
  110     format(' ',I6,1PE16.8,:,5X,'J =',I8,5X,'K =',I8,5X,'M =',I8)
  111   continue
        call MASHED ('ADELMA')
      end if
C
C     (Give back IW allotment)
      call IGIVE    (IW, 'ADELMA')
C     !END
      call BYE ('ADELMA')
C
      return
      end
