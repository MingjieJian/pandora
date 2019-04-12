      subroutine MEMEL
     $(IND,I,J,DEMJ,XKLJ,PKJJ,WNIJ,PHIJ,PAIJ,PGIJ,PBI)
C
C     Rudolf Loeser, 1980 Jan 28
C---- Dump printout, for PALLAS.
C     !DASH
      save
C     !DASH
      real*8 DEMJ, PAIJ, PBI, PGIJ, PHIJ, PKJJ, WNIJ, XKLJ
      integer I, IND, IPR01, IPR02, IPR03, IPR04, J, LUEO
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
      equivalence (KZQ( 59),IPR03)
      equivalence (KZQ( 60),IPR04)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('MEMEL')
C     !BEG
      if((IND.ge.IPR01).and.(IND.le.IPR02)) then
        if((I.ge.IPR03).and.(I.le.IPR04)) then
C
          if(J.eq.1) then
            call LINER (1, LUEO)
            write (LUEO,100)
  100       format(' ',22X,'DEM',12X,'KL',11X,'PKJ',12X,'WN',11X,'PHI',
     $                 12X,'PA',12X,'PG',12X,'PB')
          end if
C
          write (LUEO,101) I,J,DEMJ,XKLJ,PKJJ,WNIJ,PHIJ,PAIJ,PGIJ,PBI
  101     format(' ',3X,2I4,1P8E14.7)
C
        end if
      end if
C     !END
      call BYE ('MEMEL')
C
      return
      end
