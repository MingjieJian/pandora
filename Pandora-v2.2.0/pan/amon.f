      subroutine AMON
     $(NW,A,WAVE,N,YAYB,TE,RM,RD,KOOL)
C
C     Rudolf Loeser, 1971 Jun 17
C---- Computes RM and RD, for OSIRIS.
C     !DASH
      save
C     !DASH
      real*8 A, CD, CM, DX, FAC, HALF, PLANCK, RD, RDL, RDR, RM, RML,
     $       RMR, SD, SM, TE, WAVE, XL, XR, YAYB, ZERO
      integer I, IPEX, J, LUEO, N, NW
      logical DUMP, KOOL, PUMP
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
      equivalence (KZQ( 18),IPEX )
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
      equivalence (DLIT(12),HALF  )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 1),PLANCK)
C     !DASH
C     !EJECT
      external ELK, RIGEL, MESHED, MASHED, HI, BYE
C
C               A(NW), WAVE(NW), YAYB(N,NW), TE(N), RM(N), RD(N)
      dimension A(*),  WAVE(*),  YAYB(N,*),  TE(*), RM(*), RD(*)
C
      call HI ('AMON')
C     !BEG
      PUMP = (IPEX.lt.0).or.(IPEX.eq.2)
      if(PUMP) then
        call MESHED ('AMON', 2)
      end if
C
      do 104 I = 1,N
        DUMP = PUMP.and.((I.eq.1).or.(I.eq.N))
        if(DUMP) then
          write (LUEO,100) I,KOOL
  100     format(' ','AMON',I10,L18)
        end if
        SM   = ZERO
        SD   = ZERO
        call ELK    (YAYB(I,1), TE(I), WAVE(1), A(1), KOOL, XR,
     $               RMR, RDR, DUMP)
        do 102 J = 2,NW
          XL  = XR
          RML = RMR
          RDL = RDR
          call ELK  (YAYB(I,J), TE(I), WAVE(J), A(J), KOOL, XR,
     $               RMR, RDR, DUMP)
          DX = XL-XR
          CM = HALF*DX*(RML+RMR)
          CD = HALF*DX*(RDL+RDR)
          SM = SM+CM
          SD = SD+CD
          if(DUMP) then
            write (LUEO,101) J,XL,RML,RDL,XR,RMR,RDR,J,DX,CM,CD,SM,SD
  101       format(' ','AMON',I10,1P6E18.10)
          end if
  102   continue
C
        call RIGEL  (17, FAC)
        if(KOOL) then
          FAC = FAC*PLANCK
        end if
        RM(I) = FAC*SM
        RD(I) = FAC*SD
        if(DUMP) then
          write (LUEO,101) I,RM(I),RD(I),FAC,SM,SD
        end if
  104 continue
      if(PUMP) then
        call MASHED ('AMON')
      end if
C     !END
      call BYE ('AMON')
C
      return
      end
