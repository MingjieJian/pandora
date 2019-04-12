      subroutine LIDMAK
     $(XLB1,K1, XLB2,K2, XLB3,K3, INDEX)
C
C     Rudolf Loeser, 1999 Dec 07
C---- Writes part(s) of a Line Intensity Data Block.
C     (See also LIDPUT and LIDGET.)
C     !DASH
      save
C     !DASH
      real*8 XLB1, XLB2, XLB3
      integer INDEX, K1, K2, K3, LUEO, MMNAM, MMYAM, MMZAM
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 1),MMNAM)
      equivalence (MMP( 1),MMYAM)
      equivalence (MMT( 1),MMZAM)
C
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external CEBU, LIDCEK, MESHED, ABORT, HI, BYE
C
C               XLB1(Li1len), XLB2(Li2len), XLB3(Li3len)
      dimension XLB1(*),      XLB2(*),      XLB3(*)
C
      call HI ('LIDMAK')
C     !BEG
      if((INDEX.lt.1).or.(INDEX.gt.NUMTRN)) then
        call MESHED ('LIDMAK', 1)
        write (LUEO,100) INDEX,NUMTRN,K1,K2,K3
  100   format(' ','Error in writing Line Intensity Data Blocks.'//
     $         ' ','INDEX = ',I10,2X,'(NUMTRN = ',I10,')',5X,3I5)
        call ABORT
      end if
C
      if(K1.gt.0) then
        call LIDCEK ('LIDMAK', 1, XLB1(MMNAM), LINNAM(INDEX))
        call CEBU   (XLB1, LI1LEN, LI1ADR(INDEX))
      end if
C
      if(K2.gt.0) then
        call LIDCEK ('LIDMAK', 2, XLB2(MMYAM), LINNAM(INDEX))
        call CEBU   (XLB2, LI2LEN, LI2ADR(INDEX))
      end if
C
      if(K3.gt.0) then
        call LIDCEK ('LIDMAK', 3, XLB3(MMZAM), LINNAM(INDEX))
        call CEBU   (XLB3, LI3LEN, LI3ADR(INDEX))
      end if
C     !END
      call BYE ('LIDMAK')
C
      return
      end
