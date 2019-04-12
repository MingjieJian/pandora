      subroutine DIVIDE
     $(A,B,RATIO)
C
C     Rudolf Loeser, 1984 Jan 10
C---- Computes A/B.
C     (This is version 3 of DIVIDE.)
C
C     See also DIVVY.
C
C     !DASH
      save
C     !DASH
      real*8 A, B, RATIO, ZERO
      integer LUEO
C     !COM
C---- DIVIDER     as of 2003 Sep 29
      real*8      VSMLDV
      integer     MESDIV,MESDVZ,KNTDIV,KNTDVZ
      common      /DIVIDE1/ VSMLDV
      common      /DIVIDE2/ MESDIV,MESDVZ,KNTDIV,KNTDVZ
C     Parameters for subroutine "DIVIDE":
C       VSMLDV - replacement for B in A/B when B=0;
C       MESDIV - switch for A/0 error messages;
C       MESDVZ - switch for 0/0 error messages;
C       KNTDIV - number of times A/0 was detected;
C       KNTDVZ - number of times 0/0 was detected.
C     .
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      external  MESHED, MASHED, HALT, HI, BYE
      intrinsic abs
C
      call HI ('DIVIDE')
C     !BEG
      if(abs(B).gt.ZZSMALL) then
        RATIO = A/B
C
      else
        if(VSMLDV.eq.ZERO) then
          write (MSSLIN(1),100)
  100     format('VSMLDV = 0, which is nonsense.')
          call HALT     ('DIVIDE', 1)
        end if
C
        if(A.ne.ZERO) then
          RATIO  = A/VSMLDV
          KNTDIV = KNTDIV+1
          if(MESDIV.gt.0) then
            call MESHED ('DIVIDE', 2)
            write (LUEO,101) A,VSMLDV,RATIO,KNTDIV
  101       format(' ','A/B trouble: A =',1PE24.16,', B = 0, replaced ',
     $                 'by',E10.2,', Ratio =',E12.4,', KNTDIV =',I10,2X,
     $                 '*************')
            call MASHED ('DIVIDE')
          end if
C
        else
          RATIO  = ZERO
          KNTDVZ = KNTDVZ+1
          if(MESDVZ.gt.0) then
            call MESHED ('DIVIDE', 2)
            write (LUEO,102) RATIO,KNTDVZ
  102       format(' ','A/B trouble: 0/0 =',1PE12.4,', KNTDVZ =',I10,
     $                 3X,7('**********'))
            call MASHED ('DIVIDE')
          end if
        end if
C
      end if
C     !END
      call BYE ('DIVIDE')
C
      return
      end
