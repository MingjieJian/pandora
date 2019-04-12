      subroutine NOBLE
     $(LODE,NL,IL,XND,P,IU,XQ,TND,ITAU,KILROY,CALLER)
C
C     Rudolf Loeser, 2003 Aug 11
C---- Prints dump for SOD.
C     !DASH
      save
C     !DASH
      real*8 P, TND, XND, XQ
      integer IL, IS, ITAU, IU, LODE, LUEO, NL
      logical KILROY
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, LEBON, HI, BYE
C
C               XND(NL), P(NL)
      dimension XND(*),  P(*)
C
      call HI ('NOBLE')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call MESHED  (CALLER, 2)
        write (LUEO,100)
  100   format(' ','Details of NEDIT.')
        IS = 0
      end if
      if(IS.ne.ITAU) then
        IS = ITAU
        call LINER   (2, LUEO)
        write (LUEO,101) IS
  101   format(' ','Depth # =',I6)
      end if
      if(LODE.gt.0) then
        if(LODE.eq.1) then
          call LINER (1, LUEO)
          write (LUEO,102) IL
  102     format(' ','l =',I3)
          call LEBON (XND, NL, 'N')
          call LEBON (P  , NL, 'P')
        end if
        write (LUEO,103) IU,XQ,TND
  103   format(' ',6X,'u =',I3,2X,'Q =',1PE14.6,3X,'Nnew =',E14.6)
      else
        call LEBON   (XND, NL, 'N')
      end if
C     !END
      call BYE ('NOBLE')
C
      return
      end
