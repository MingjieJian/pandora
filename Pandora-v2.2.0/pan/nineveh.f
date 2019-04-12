      subroutine NINEVEH
     $(KOMPO,NP,TABP,NT,TABT,NV,TABV)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Reads pressure, temperature and velocity grids, for AMBRO.
C     !DASH
      save
C     !DASH
      real*8 TABP, TABT, TABV, TWO
      integer I, ISKIP, KOMPO, LUEO, LV, MP, MT, MV, NP, NT, NV, NWAV
C     !COM
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
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  MESHED, ABORT, HI, BYE
      intrinsic max
C
C               TABP(NP), TABT(NT), TABV(NV)
      dimension TABP(*),  TABT(*),  TABV(*)
C     !EJECT
C
      call HI ('NINEVEH')
C     !BEG
      rewind KOMPO
C
      read  (KOMPO,100) NWAV,MP,MT,LV
  100 format(4I20)
      MV = max(LV,1)
C
      ISKIP = (NWAV+3)/4
      do 102 I = 1,ISKIP
        read (KOMPO,101)
  101   format(4E20.12)
  102 continue
C
      if((MP.ne.NP).or.(MT.ne.NT).or.(MV.ne.NV)) then
        call MESHED ('NINEVEH',1)
        write (LUEO,103) KOMPO,MP,NP,MT,NT,MV,NV,LV
  103   format(' ','Trouble reading Composite Line Opacity data from ',
     $             'file',I4,'.'//
     $         ' ','MP',I5,5X,'NP',I5,10X,'MT',I5,5X,'NT',I5,10X,'MV',
     $             I5,5X,'NV',I5,5X,'LV',I5)
        call ABORT
      end if
C
      if(LV.gt.0) then
        read (KOMPO,101) (TABP(I),I=1,MP),(TABT(I),I=1,MT),
     $                   (TABV(I),I=1,MV)
      else
        read (KOMPO,101) (TABP(I),I=1,MP),(TABT(I),I=1,MT)
        TABV(1) = TWO
      end if
C     !END
      call BYE ('NINEVEH')
C
      return
      end
