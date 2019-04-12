      subroutine BOLA
     $(LDL,DDL,CDW,L,OFF,DELTA,IPNT)
C
C     Rudolf Loeser, 1989 Feb 08
C---- Makes a sorted table of distinct component offsets.
C     (This is version 2 of BOLA.)
C     !DASH
      save
C     !DASH
      real*8 CDW, DDL, DELTA, OFF
      integer I, IFLG, IPNT, L, LDL
C     !DASH
      external MOVE1, CONDIV, SORT, COMPD, HI, BYE
C
C               DDL(LDL), OFF(LDL), IPNT(LDL)
      dimension DDL(*),   OFF(*),   IPNT(*)
C
      call HI ('BOLA')
C     !BEG
C---- Set up offsets
      call MOVE1   (DDL, LDL, OFF)
      call CONDIV  (CDW, OFF, LDL)
C
C---- Sort into increasing order
      call SORT    (OFF, LDL, IPNT, 'Blended-line offsets')
C
C---- Eliminate non-distinct values
      L = 1
      do 100 I = 2,LDL
        call COMPD (OFF(I), OFF(L), DELTA, IFLG)
        if(IFLG.ne.0) then
          L = L+1
          OFF(L) = OFF(I)
        end if
  100 continue
C     !END
      call BYE ('BOLA')
C
      return
      end
