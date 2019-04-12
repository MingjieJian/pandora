      subroutine ARMILLA
     $(WSM,IFS,ILS)
C
C     Rudolf Loeser, 1982 Nov 29
C---- Reads Rho smoothing parameters.
C     (This is version 2 of ARMILLA.)
C     !DASH
      save
C     !DASH
      real*8 WSM, dummy
      integer IFS, ILS, KERR, KIND, LOOK, LUEO, MODE, NTAB, jummy
      character NAMES*8, QNAME*8, RPAREN*1, qummy*8
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(50),RPAREN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MACE, KIWI, LOOKUC, MESHED, CHLOE, ABORT, UNMIX, CARMEN,
     $         HI, BYE
C
      dimension NAMES(3)
C
      data NTAB  /3/
      data NAMES /'WSM', 'IFS', 'ILS'/
C     !EJECT
C
      call HI ('ARMILLA')
C     !BEG
      KERR = 0
      call MACE
  100 continue
        call KIWI       (MODE, dummy, jummy, QNAME, jummy)
        if(MODE.ne.2) goto 201
        call UNMIX      (QNAME)
        call LOOKUC     (NAMES, NTAB, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          if(KIND.eq.1) then
            call KIWI   (MODE, WSM, jummy, qummy, jummy)
            if(MODE.ne.5) goto 204
            goto 100
          else
            if(KIND.eq.2) then
              call KIWI (MODE, dummy, IFS, qummy, jummy)
            else
              call KIWI (MODE, dummy, ILS, qummy, jummy)
            end if
            if(MODE.ne.3) goto 203
            goto 100
          end if
        end if
C
        if(QNAME.ne.RPAREN) goto 202
        goto 199
C
C---- Errors
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED       ('ARMILLA', 1)
      write (LUEO,200) NAMES,RPAREN
  200 format(' ','Trouble reading Rho smoothing parameters. List of ',
     $           'valid controls fields:'//
     $      (' ',4X,10A10))
      call CHLOE        (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('ARMILLA')
C
      return
      end
