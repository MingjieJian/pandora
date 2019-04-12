      subroutine RONGA
     $(N,TE,XNC,EN,WN,DN,AN,SA,SF,XLM)
C
C     Rudolf Loeser, 2003 Jan 08
C---- Initializes H Ly lines data.
C     !DASH
      save
C     !DASH
      real*8 AN, CLIGHT, CNC, CSK, CTE, DN, EN, EN3, FAC, FCT, ONE,
     $       PMSK, SA, SF, T, TE, TIN, TOUT, WN, XLM, XNC, XNU, ZERO,
     $       dummy1, dummy2
      integer I, IPEX, J, JHLSK, K, LUEO, M, N
      logical DUMP
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
      equivalence (RZQ(124),PMSK )
      equivalence (KZQ(190),JHLSK)
      equivalence (KZQ( 18),IPEX )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 3),CLIGHT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !EJECT
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C     !DASH
      external KERBAU, BUREKA, HYDATA, NERONE, SECOND, MESHED, MASHED,
     $         DACAPO, KITH, HI, BYE
C
C               WN(NLL), DN(NLL), EN(NLL), AN(NLL,N), SA(NLL,N), TE(N),
      dimension WN(*),   DN(*),   EN(*),   AN(NLL,*), SA(NLL,*), TE(*),
C
C               SF(NLL), XNC(N)
     $          SF(*),   XNC(*)
C
      data FAC,FCT /1.0001D0, 1.D-22/
      data CTE,CNC /0.D0, 0.D0/
C
      call HI ('RONGA')
C     !BEG
      if(INIHLL) then
        INIHLL = .false.
C
        DUMP = (IPEX.lt.0).or.(IPEX.eq.14)
        if(DUMP) then
          M = NLL-1
          call DACAPO     (XLM)
          call MESHED     ('RONGA', 2)
          write (LUEO,100) XLM
  100     format(' ','RONGA; XLM =',1PE24.16)
          write (LUEO,101) 2,EN(2),WN(2),DN(2),AN(2,1),SA(2,1),SF(2),
     $                     M,EN(M),WN(M),DN(M),AN(M,1),SA(M,1),SF(M)
  101     format(' ',I5,1P6E20.12)
          call SECOND     (TIN)
        end if
C     !EJECT
        M = NLL-1
        J = NLL+1
        do 103 I = 1,M
          J = J-1
C
          EN(I) = J
          call KITH       (J, WN(I))
C
          EN3 = J**3
          DN(I) = ONE/EN3
C
          if(INDPTH) then
            do 102 K = 1,N
              call KERBAU (J, 1, TE(K), XNC(K), AN(I,K))
              call BUREKA (J,    TE(K), XNC(K), SA(I,K))
  102       continue
          else
            call KERBAU   (J, 1, CTE,   CNC   , AN(I,1))
            call BUREKA   (J,    CTE,   CNC   , SA(I,1))
          end if
C
          if(JHLSK.gt.0) then
            call HYDATA   (J, XNU, dummy1, dummy2)
            call NERONE   (J, 1, PMSK, CSK)
            SF(I) = ((XNU**2)/(CLIGHT*FCT))*CSK
          else
            SF(I) = ZERO
          end if
  103   continue
C
        EN(NLL) = ONE
        DN(NLL) = ONE
        WN(NLL) = FAC*WN(M)
        SF(NLL) = FAC*SF(M)
        if(INDPTH) then
          do 104 K = 1,N
            AN(NLL,K) = FAC*AN(M,K)
            SA(NLL,K) = FAC*SA(M,K)
  104     continue
        else
          AN(NLL,1) = FAC*AN(M,1)
          SA(NLL,1) = FAC*SA(M,1)
        end if
C     !EJECT
        if(DUMP) then
          call SECOND (TOUT)
          T = TOUT-TIN
          write (LUEO,105) T
  105     format(' ','RONGA; T =',F10.5)
          write (LUEO,101) 2,EN(2),WN(2),DN(2),AN(2,1),SA(2,1),SF(2),
     $                     M,EN(M),WN(M),DN(M),AN(M,1),SA(M,1),SF(M)
          call MASHED ('RONGA')
        end if
C
      end if
C     !END
      call BYE ('RONGA')
C
      return
      end
