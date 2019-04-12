      subroutine CORSICA
     $(DUMP,NAME,ISWA,INDX,NOPAC,N,CO,CB,ML,CABS,CEMI,KEMIT)
C
C     Rudolf Loeser, 2004 Jun 23
C---- Final processing for sets of background lines.
C     (This is version 2 of CORSICA.)
C     !DASH
      save
C     !DASH
      real*8 CABS, CB, CEMI, CO, SUMB, SUMO, ZERO
      integer I, INDX, ISWA, KEMIT, L, ML, N, NOPAC
      logical DUMP
      character NAME*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DRITTEL, HI, BYE
C
C               CABS(N,Nlin), CEMI(N,Nlin), CB(Nopac,N), CO(Nopac,N),
      dimension CABS(N,*),    CEMI(N,*),    CB(NOPAC,*), CO(NOPAC,*),
C
C               ISWA(Nopac)
     $          ISWA(*)
C
      call HI ('CORSICA')
C     !BEG
      if(ISWA(INDX).gt.0) then
C
        do 101 I = 1,N
          SUMO = ZERO
          SUMB = ZERO
          do 100 L = 1,ML
            SUMO = SUMO+CABS(I,L)
            SUMB = SUMB+CABS(I,L)*CEMI(I,L)
  100     continue
          CO(INDX,I) = SUMO
          CB(INDX,I) = SUMB
  101   continue
C
        if(DUMP) then
          call DRITTEL (NAME, INDX, NOPAC, N, CO, CB, ML, CABS, CEMI,
     $                  KEMIT)
        end if
C
      end if
C     !END
      call BYE ('CORSICA')
C
      return
      end
