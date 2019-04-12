      subroutine MOAN
     $(N,NOPAC,ORES,OREM,SRES,SREM,T1,TR,S1,SR,UPO,CO,UPE,CB,
     $ KAPPA,KRESN,KODE,KUPT)
C
C     Rudolf Loeser, 2005 Oct 04
C---- Shuffles absorbers and emitters into proper slots.
C
C     KAPPA = number of current absorber
C     KRESN = number of desired absorber (for saving components)
C
C     KODE = 3 for current line (PRD, FDB)
C     KODE = 2 for Lyman
C     KODE = 1 for everything else
C
C     The input switches UPO and UPE control the calculations of the
C     current round:
C                        UPO = "UPdate the Opacity"
C                        UPE = "UPdate the Emission"
C
C     In the Lyman context, components have to be kept separate and
C     their totals set = 0 temporarily. This is OK because, in a Lyman
C     run, the ion-of-the-run is also a population ion, and that ion's
C     b-f contribution always has to be updated. The output switch
C     KUPT ("UPdate the Totals") is set = KAPPA if the totals have been
C     temporarily set =0 here; KUPT must be initialized by the caller.
C     (The updating happens in BUSTARD and MATACO.)
C
C     (This is version 4 of MOAN.)
C     !DASH
      save
C     !DASH
      real*8 CB, CO, OREM, ORES, S1, SR, SREM, SRES, T1, TR, ZERO
      integer J, KAPPA, KODE, KRESN, KUPT, N, NOPAC
      logical UPE, UPO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external HI, BYE
C
C               ORES(N), OREM(N), SRES(N), SREM(N), T1(N), TR(N),
      dimension ORES(*), OREM(*), SRES(*), SREM(*), T1(*), TR(*),
C
C               S1(N), SR(N), CO(Nopac,N), CB(Nopac,N)
     $          S1(*), SR(*), CO(NOPAC,*), CB(NOPAC,*)
C
      call HI ('MOAN')
C     !BEG
      if((KRESN.eq.KAPPA).and.(KODE.eq.2)) then
C
C----   Need to save the components (and to make sure that the
C       totals = 0 temporarily).
        do 100 J = 1,N
          if(UPO) then
            T1(J)       = ORES(J)
            TR(J)       = OREM(J)
            CO(KAPPA,J) = ZERO
          end if
          if(UPE) then
            S1(J)       = SRES(J)*ORES(J)
            SR(J)       = SREM(J)*OREM(J)
            CB(KAPPA,J) = ZERO
          end if
  100   continue
        KUPT = KAPPA
C
      else
C
C----   Don't need the components, just compute and save totals
        do 101 J = 1,N
          if(UPO) then
            CO(KAPPA,J) = ORES(J)+OREM(J)
          end if
          if(UPE) then
            CB(KAPPA,J) = ORES(J)*SRES(J)+OREM(J)*SREM(J)
          end if
  101   continue
C
      end if
C     !END
      call BYE ('MOAN')
C
      return
      end
