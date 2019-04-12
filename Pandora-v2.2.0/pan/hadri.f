      subroutine HADRI
     $(N,NL,IQINC,IQIFF,IQFIN,IQREF,IQGDS,IQEBI)
C
C     Rudolf Loeser, 1981 Jul 31
C---- Checks some processing options for compatibility.
C     (This is version 2 of HADRI.)
C     !DASH
      save
C     !DASH
      integer IQEBI, IQFIN, IQGDS, IQIFF, IQINC, IQREF, LUEO, N, NL
      logical KILROY, STOP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MUSHED, ABORT, HI, BYE
C
      data KILROY /.true./
C
      call HI ('HADRI')
C     !BEG
      STOP = .false.
C
      if(IQFIN.le.0) then
        if((IQINC.gt.0).and.(IQIFF.le.0)) then
          call MUSHED ('HADRI', 1, KILROY)
          write  (LUEO,100)
  100     format(' ','INCIDNT on with INCIFRNT off while FINITE off ',
     $               'is not OK.')
          STOP = .true.
        end if
C
        if(IQREF.gt.0) then
          call MUSHED ('HADRI', 1, KILROY)
          write  (LUEO,101)
  101     format(' ','REFLECT on while FINITE off is not OK.')
          STOP = .true.
        end if
      end if
C     !EJECT
      if(IQEBI.gt.0) then
        if(IQGDS.gt.0) then
          call MUSHED ('HADRI', 1, KILROY)
          write (LUEO,102)
  102     format(' ','GDS on with EMERBACK on will give ',
     $               'inconsistent results.')
        end if
C
        if(IQREF.gt.0) then
          call MUSHED ('HADRI', 1, KILROY)
          write (LUEO,103)
  103     format(' ','REFLECT on with EMERBACK on does not give ',
     $               'meaningful results.')
        end if
      end if
C
      if(STOP) then
        call ABORT
      end if
C     !END
      call BYE ('HADRI')
C
      return
      end
