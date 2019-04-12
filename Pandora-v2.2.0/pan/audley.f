      subroutine AUDLEY
     $(BNAME,I,KALLER)
C
C     Rudolf Loeser, 1982 Dec 01
C---- Checks a DIANA/ORION Data Record and aborts on error.
C     !DASH
      save
C     !DASH
      real*8 BNAME
      integer I, IRAY, IRB, LLB, LLXI, LUEO, NRAY, NRB
      character KALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- ARCHER      as of 2004 May 12
      integer     NNKOD, NNKODS
      parameter   (NNKOD=3)
C     (Be sure to recompile POD when changing NNKOD.)
      dimension   NNKODS(NNKOD)
      common      /ARCHER/ NNKODS
C     Diana/Orion Data Blocks control parameters.
C
C     (These parameters are packed and unpacked by "POD".)
C       LLXI - frequency index.
C       IRAY - angle or ray index (Orion only: expanding atmosphere);
C              (in the spherical case, Shell rays are enumerated first,
C              followed by Disk rays).
C       NRAY - number of depth levels intersected by this ray;
C              (differs from N only for Shell rays).
C     .
      equivalence
     $(NNKODS( 1),LLXI  ),(NNKODS( 2),IRAY  ),(NNKODS( 3),NRAY  )
C     .
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C     !DASH
C     !EJECT
      external POD, MESHED, ISABEL, ABORT, HI, BYE
C
      call HI ('AUDLEY')
C     !BEG
      call POD      (2, BNAME)
C
      if(OPNAM(I).ne.BNAME) then
        LLB = LLXI
        IRB = IRAY
        NRB = NRAY
        call POD    (2, OPNAM(I))
        call MESHED ('AUDLEY', 1)
        write (LUEO,100) I,KALLER,IUOP,ILOP,OPNAM(I),LLXI,IRAY,NRAY,
     $                                      BNAME   ,LLB ,IRB ,NRB
  100   format(' ','Trouble reading ',I6,'. ',A,' Data Block for ',
     $             'transition (',I2,'/',I2,').'//1P,
     $         ' ','OPNAM (from INDEX)',E30.16,3I15//
     $         ' ','BNAME (as read)   ',E30.16,3I15)
        call ISABEL
        call ABORT
      end if
C     !END
      call BYE ('AUDLEY')
C
      return
      end
