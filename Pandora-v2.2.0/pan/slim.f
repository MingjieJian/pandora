      subroutine SLIM
     $(IU,IL,INDEX,GOOD)
C
C     Rudolf Loeser, 1992 Feb 20
C---- Looks up a transition in the THULE index.
C     (This is version 2 of SLIM.)
C     !DASH
      save
C     !DASH
      integer IL, INDEX, IU, LOOK, NAME, NOTE
      logical GOOD
C     !COM
C---- THULE       as of 1999 Dec 07
      integer     MAXLV,MXTRA
      parameter   (MAXLV=50)
      parameter   (MXTRA=(MAXLV*(MAXLV-1))/2)
C     (Remember to recompile ADAM when changing MAXLV.)
      integer     LMTRA,NUMTRN,NUMBLK,LINNAM,LI1ADR,LI2ADR,LI3ADR
      dimension   LI1ADR(MXTRA),LI2ADR(MXTRA),LI3ADR(MXTRA)
      dimension   LINNAM(MXTRA)
      common      /THULE0/ LMTRA,NUMTRN,NUMBLK
      common      /THULE1/ LINNAM
      common      /THULE2/ LI1ADR
      common      /THULE3/ LI2ADR
      common      /THULE4/ LI3ADR
C
C     Indices and Names of the Line Intensity Data Blocks.
C     LMTRA  -    = MAXLV
C     NUMTRN -    number of transitions (i.e. AIJ .ne. 0), which is
C                 the number of Line Intensity Data Blocks allocated;
C     NUMBLK -    number of Line Intensity Data Blocks in actual use
C                 (i.e. number of radiative and passive transitions);
C     LINNAM -    block name, = 100*IU+IL;
C     LI1ADR -    block address in scratch memory, part 1;
C     LI2ADR -    block address in scratch memory, part 2;
C     LI3ADR -    block address in scratch memory, part 3.
C     .
C     !DASH
      external LOOKSI, HI, BYE
C
      call HI ('SLIM')
C     !BEG
      INDEX = 0
      NAME  = 100*IU+IL
      call LOOKSI (LINNAM,NUMTRN,0,NAME,INDEX,NOTE,LOOK)
      if(LOOK.eq.2) then
        INDEX = NUMTRN
      end if
      GOOD = ((LOOK.eq.1).and.(NOTE.eq.1)).or.(LOOK.eq.2)
C     !END
      call BYE ('SLIM')
C
      return
      end
