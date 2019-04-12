      subroutine UHU
     $(KTYPE,REG,FDB,PRD,LINE)
C
C     Rudolf Loeser, 2006 Dec 14
C---- Analyzes KTYPE, to see what kind of a line this is.
C     !DASH
      save
C     !DASH
      integer KTYPE
      logical FDB, LINE, PRD, REG
C     !DASH
      external HI, BYE
C
      call HI ('UHU')
C     !BEG
      REG = (KTYPE.eq. 1).or.(KTYPE.eq.19)
      FDB = (KTYPE.eq.16).or.(KTYPE.eq.18).or.(KTYPE.eq.26)
      PRD = (KTYPE.eq. 4).or.(KTYPE.eq.24).or.(KTYPE.eq.25)
C
      LINE = REG.or.FDB.or.PRD
C     !END
      call BYE ('UHU')
C
      return
      end
