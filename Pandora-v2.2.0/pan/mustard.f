      subroutine MUSTARD
     $(QNAME,ARRAY,IRRAY,CARRAY,K,MEXP)
C
C     Rudolf Loeser, 1968 Apr 19
C---- Reads arrays of length 1.
C     !DASH
      save
C     !DASH
      real*8 ARRAY, VD
      integer IRRAY, IV, K, KERR, LUEO, MEXP, MODE, NC
      character CARRAY*(*), QNAME*8, VC*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MACE, MICE, KIWI, MESHED, ABORT, CARMEN, CHLOE, HI, BYE
C
C     (The sizes of ARRAY, IRRAY and CARRAY must be large enough to
C     accommodate the subcript K.)
C
      dimension ARRAY(*), IRRAY(*), CARRAY(*)
C
C     !EJECT
C
      call HI ('MUSTARD')
C     !BEG
      call MACE
      call KIWI  (MODE, VD, IV, VC, NC)
C
      KERR = 0
      if(MODE.eq.2) then
        if(MODE.ne.MEXP) then
          goto 205
        else
          CARRAY(K) = VC
        end if
      else if(MODE.eq.3) then
        if(MODE.ne.MEXP) then
          goto 203
        else
          IRRAY(K) = IV
        end if
      else if(MODE.eq.5) then
        if(MODE.ne.MEXP) then
          goto 204
        else
          ARRAY(K) = VD
        end if
      end if
C
      call MICE
      goto 199
C
C---- Error processing
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('MUSTARD', 1)
      write (LUEO,200) QNAME,MODE,MEXP
  200 format(' ','Trouble reading for ',A10,'.  KIWI code, actual =',
     $           I12,', expected =',I3)
      call CHLOE  (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
C
  199 continue
C     !END
      call BYE ('MUSTARD')
C
      return
      end
