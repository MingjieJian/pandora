      subroutine SHIRT
     $(NDW,ZNDW,Z,KZXST,N,NMLR)
C
C     Rudolf Loeser, 2004 Aug 11
C---- Makes sure NDW is ok.
C     (This was HEGRU from 1988 Mar 31.)
C     (This is version 2 of SHIRT.)
C     !DASH
      save
C     !DASH
      real*8 Z, ZERO, ZNDW
      integer IDW, KIND, KZXST, LUEO, N, NDW, NMLR
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
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external NOTMORE, MESHED, MASHED, HI, BYE
C
C               Z(N)
      dimension Z(*)
C
      call HI ('SHIRT')
C     !BEG
      if((ZNDW.ne.ZERO).and.(KZXST.gt.0)) then
        call NOTMORE  (Z, N, ZNDW, KIND)
        if((KIND.ge.1).and.(KIND.lt.N)) then
          if((ZNDW-Z(KIND)).gt.(Z(KIND+1)-ZNDW)) then
            NDW = KIND+1
          else
            NDW = KIND
          end if
        else
          NDW = KIND
        end if
      end if
C
      if((NDW.lt.1).or.(NDW.gt.N)) then
        IDW = NDW
        NDW = N/2
        if(IDW.ne.-1) then
          call MESHED ('SHIRT', 3)
          write (LUEO,100) IDW,NDW
  100     format(' ','NDW =',I12,' does not make sense; it has been ',
     $               'reset to NDW =',I12,'.')
          call MASHED ('SHIRT')
        end if
      end if
C
      if(NMLR.le.0) then
        NMLR = NDW
      end if
C     !END
      call BYE ('SHIRT')
C
      return
      end
