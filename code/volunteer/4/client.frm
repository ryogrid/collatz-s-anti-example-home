[form1.frm]





Option Explicit
Public mx As Double
Public my As Double
Dim muPreserved As Preserved
Dim mblnEndFlag As Boolean             '�I���������s���ėǂ����̃t���O
Dim mintFoultCount As Integer          '�ڑ������s�����񐔂�ێ�
Dim mblnConnectionFlag As Boolean      '�ʐM���\���i�ڑ�������ɉ��Ă��邩�j�̃t���O
Dim lngProgramStartTime As Long         '�v���O�������N������������ێ��i�v���O�������N�����ԎZ�o�p�j
Dim nidSysInfo As NOTIFYICONDATA
Dim lRetVal As Long
Dim blnFormVisibleFlag As Boolean       '��ʏ�Ƀt�H�[�����\������Ă��邩�������t���O
Dim lngProcessID As Long                '�r���������֐���p����ꍇ�̖߂�l�ł���v���Z�X�h�c��ێ����Ă���
Dim mHwnd As Long                       '�t�H�[���̃n���h����ێ�
Dim blnPassFlag As Boolean               '�v�Z�J�n�̂h�e���𔲂��邽�߂�flag

Private Sub Form_Load()
Dim intFilenumber
Dim work As Integer
Dim ProcessID As Long
      
        
        ProcessID = GetCurrentProcess                      '���v���Z�X�̃v���Z�X�h�c�𓾂�
        Call SetPriorityClass(ProcessID, IDLE_PRIORITY_CLASS)  '���v���Z�X�̗D��x��������
        
        mHwnd = Form1.hwnd    '�t�H�[���̃n���h�����擾����
        'shell_notifyicon�`�o�h�̂��߂̏����l�ݒ�
        nidSysInfo.cbSize = Len(nidSysInfo)                        '�\���̂̃T�C�Y
        nidSysInfo.hwnd = Form1.Picture1.hwnd                                  '�ʒm�����E�B���h�E�n���h��
        nidSysInfo.uID = 1                                         'ID
        nidSysInfo.uFlags = NIF_ICON Or NIF_TIP Or NIF_MESSAGE     '�t���O
        nidSysInfo.uCallbackMessage = WM_MBUTTONDOWN               'Calllback Message
        nidSysInfo.hIcon = Me.Icon                                 '�^�X�N�g���C�ɕ\�������A�C�R��
        nidSysInfo.szTip = "Collatz's Anti-Exp����������" & vbNullChar                   'ToolTip�ɕ\������镶����
        Form1.Visible = False
        lRetVal = Shell_NotifyIcon(NIM_ADD, nidSysInfo)            '�^�X�N�g���C�ɃA�C�R�����i�[����
        
        lngProgramStartTime = Timer
        Call LoadData
        If SpaceTrim(muPreserved.strName) = "" Then
                  work = MsgBox("�����ݒ�����j���[����s���ĉ�����", vbOKOnly + vbExclamation, "Collatz's Anti-Exp����������")
                  Call ShowForm
                  mblnEndFlag = True
                  Call mnuPrepare_Click
                  Exit Sub
        End If
        If muPreserved.blnAutoStartFlag = False And blnPassFlag = False Then              '�����v�Z�������Ă��Ȃ��ꍇ
                  work = MsgBox("�v�Z���J�n���Ă�낵���ł����H", vbYesNo, "Collatz's Anti-Exp����������")
                  If work = vbNo Then
                        mblnEndFlag = True
                        Label3.Caption = "�ҋ@��"
                        Label3.Left = (Form1.Width - Label3.Width) / 2
                        mnuExecute.Visible = True
                        Exit Sub
                  End If
        End If
        Winsock1.Close                                '�ڑ������
        Winsock1.RemotePort = 1001                    '�|�[�g�ԍ��ݒ�
        Winsock1.LocalPort = 0

        If Left(muPreserved.strMustCulculate, 1) = "#" Or muPreserved.blnCulculateComplete = True Then
            mblnEndFlag = False
            Call Connect
        Else
            Call Calculate        '�v�Z���s��
        End If
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
        
        If UnloadMode = 0 And mblnEndFlag = False Then        '����{�^���ŕ����āA���Amblnendflag=false�̏ꍇ�I�����L�����Z������
               Cancel = 1
               Exit Sub
        End If
        Call EndWorking
End Sub

Private Sub mnuExecute_Click()
        blnPassFlag = True           'if���𔲂�����悤�ɂ���
        mnuExecute.Visible = False
        Call Form_Load
End Sub

Private Sub mnuPrepare_Click()
Dim intFilenumber As Integer
Dim muPreserved As Preserved
Dim strexist As String
Dim intreturn As Integer
Dim work As Integer
                 
       If mblnEndFlag = True Then
            Timer3.Enabled = False
'            muPreserved.strAddres = InputBox("�T�[�o�̃A�h���X����͂��ĉ�����", "�A�h���X�̎w��")
            muPreserved.strAddres = "myhomegrid.myhome.cx"
            muPreserved.strName = InputBox("���O�C�������鎞�̃j�b�N�l�[������͂��ĉ�����", "�j�b�N�l�[���̓o�^")
            muPreserved.strMustCulculate = "#"                '�܂��v�Z���ׂ��l�𓾂Ă��Ȃ����������悤�ɏ�������
            work = MsgBox("����A�����Ōv�Z���s�����������܂����H", vbYesNo, "�����v�Z����")
            If work = vbYes Then
                   muPreserved.blnAutoStartFlag = True
            End If
            work = MsgBox("�����ڑ��������܂����H", vbYesNo, "�����ڑ�����")
            If work = vbYes Then
                   muPreserved.blnAutoConnectConsent = True
            End If
            work = MsgBox("�p�\�R���̃X�y�b�N���̑��M�������܂����H", vbYesNo, "�p�\�R����񑗐M����")
            If work = vbYes Then
                   muPreserved.blnSpecSendConsent = True
            End If
            muPreserved.strMassageTo = InputBox("�T�[�o�Ǘ��҂Ƀ��b�Z�[�W������΂�������������(�R�O�����ȓ��j", "���b�Z�[�W�L�^", "�Ȃ�")
            intreturn = MsgBox("�f�[�^�t�@�C�������������Ă��܂��܂�����낵���ł����H", vbOKCancel + vbCritical)
            If intreturn = vbOK Then
                Call Form1.FileCodeOut("Registry", xORnumber)
                intFilenumber = FreeFile
                Open App.Path & "\" & "Registry" For Random As intFilenumber Len = Len(muPreserved)
                    Put #intFilenumber, 1, muPreserved
                Close #intFilenumber
                Call Form1.FileCodeIn("Registry", xORnumber)             '�t�@�C�����Í�������
                mblnEndFlag = False
                Call Form_Load
            End If
       End If
End Sub

Private Sub mnuVeryHigh_Click()                        '�D��x�@"����"���N���b�N��
Dim ProcessID As Long
        
        mnuHigh.Checked = False
        mnuNormal.Checked = False
        mnuVeryHigh.Checked = True
        ProcessID = GetCurrentProcess                      '���v���Z�X�̃v���Z�X�h�c�𓾂�
        Call SetPriorityClass(ProcessID, HIGH_PRIORITY_CLASS)  '���v���Z�X�̗D��x��ύX����
        Call ChangePriorityC(HIGH_PRIORITY_CLASS)   '�b����̂d�w�d���N������Ă���ꍇ��������ύX����
End Sub

Private Sub mnuHigh_Click()                              '�D��x�@"��������"���N���b�N��
Dim ProcessID As Long
        
        mnuNormal.Checked = False
        mnuVeryHigh.Checked = False
        mnuHigh.Checked = True
        ProcessID = GetCurrentProcess                      '���v���Z�X�̃v���Z�X�h�c�𓾂�
        Call SetPriorityClass(ProcessID, NORMAL_PRIORITY_CLASS)  '���v���Z�X�̗D��x��ύX����
        Call ChangePriorityC(NORMAL_PRIORITY_CLASS)   '�b����̂d�w�d���N������Ă���ꍇ��������ύX����
End Sub

Private Sub mnuNormal_Click()                             '�D��x�@"�ʏ�"���N���b�N��
Dim ProcessID As Long
        
        mnuVeryHigh.Checked = False
        mnuHigh.Checked = False
        mnuNormal.Checked = True
        ProcessID = GetCurrentProcess                      '���v���Z�X�̃v���Z�X�h�c�𓾂�
        Call SetPriorityClass(ProcessID, IDLE_PRIORITY_CLASS)  '���v���Z�X�̗D��x��ύX����
        Call ChangePriorityC(IDLE_PRIORITY_CLASS)   '�b����̂d�w�d���N������Ă���ꍇ��������ύX����
End Sub


Private Sub Timer2_Timer()                   '1���o�ߌ�ɐڑ��v�������݂�^�C�}�[
        Timer2.Enabled = False
        mblnEndFlag = False
        Call Connect
End Sub

Private Sub Timer3_Timer()                 '�f�[�^���M���K�؂ɐ����ꂽ���Ď�����^�C�}�[(�ԐM���Ȃ��ꍇ�A���̃^�C�}�[�ɂ���ċ����I������)
Dim work As Integer
        
        If mintFoultCount >= 5 Then                    '���łɂT��ڑ������s���Ă��܂��Ă���ꍇ
            work = MsgBox("�T�[�o�[���������Ȃ����ߏI�����܂�", vbOKOnly, "Collatz's Anti-Exp����������")
            Call EndWorking
        Else
            Label3.Caption = "�ڑ����s�E�ҋ@��"
            Label3.Left = (Form1.Width - Label3.Width) / 2
            Timer2.Enabled = True                      '�ꕪ��ɍĂѐڑ������݂�^�C�}�[���N������
            mintFoultCount = mintFoultCount + 1
            mblnEndFlag = True
        End If
        Timer3.Enabled = False
End Sub

Private Sub Winsock1_Connect()
        mblnConnectionFlag = True
End Sub

Private Sub Winsock1_DataArrival(ByVal bytesTotal As Long)
Dim strDat As String
Dim intFilenumber As Integer
Dim valsplited As Variant
Static stcstrdat As String
Dim strexist As String

On Error Resume Next
        Winsock1.GetData strDat    '�T�[�o����̃f�[�^��M
        stcstrdat = stcstrdat & strDat
             If Right(strDat, 1) = "#" Then
                strDat = Left(stcstrdat, Len(stcstrdat) - 1)
                    strDat = OutCode(strDat, xORnumber)       '�f�[�^�𕜍�������
                    If Left(strDat, 2) = "##" Then
                        Winsock1.Close
                        Timer3.Enabled = False         '�����I���^�C�}�[���~����
                        valsplited = Split(strDat, "!")
                        muPreserved.intFilenumber = Val(valsplited(1))
                        Winsock1.LocalPort = 0
                        muPreserved.blnCulculateComplete = False       '�v�Z�͏I�����Ă��Ȃ��i���ꂩ�炾�Ƃ��������L�^)
                        muPreserved.strMustCulculate = SpaceTrim(CStr(valsplited(2)))                          '�v�Z���s���l����
                        muPreserved.intWorkCounts = CInt(valsplited(3))
                        muPreserved.intPrize = CInt(valsplited(4))
                        muPreserved.lngNowProgress = 0
                        Winsock1.Close
                        Label4.Caption = muPreserved.intWorkCounts & "��ڂ̃��[�N�ł��F�f�[�^�擾���̏��ʁ@" & muPreserved.intPrize & "��"
                        Label4.FontSize = Form1.Height * (72 / 1440) / 20
                        Label4.Left = 0
                        Label4.Top = 0
                        Call FileCodeOut("CulculateData", xORnumber)
                        strexist = Dir(App.Path & "\" & "CulculateData")            '�v�Z���ʂ̓r���o�߂̃t�@�C�������݂���ꍇ�폜���Ă���
                        If strexist = "CulculateData" Then
                                  Kill App.Path & "\" & "CulculateData"
                        End If
                        Call Calculate
                    End If
                stcstrdat = ""
             End If
End Sub

Private Sub Timer1_Timer()                    '�b����̂d�w�d���v�Z���I�����������Ď�����^�C�}�[�R���g���[��
Dim strWrited As String
Dim strCompleted As String
Dim intFilenumber As Integer
Dim lngNowProgress As Long
Dim work As Integer
Dim TimerStartTime As Long                    '�^�C�}�[�̏������n�܂���������ێ�

         On Error GoTo Errorhandler
         TimerStartTime = Timer
         Do While (Timer - TimerStartTime < 0.5)        '0.5�b�ԁA�n�r�ɐ����߂�
          DoEvents
         Loop
         intFilenumber = FreeFile
         Open App.Path & "\" & "CulculateData" For Random As intFilenumber Len = Len(lngNowProgress)
                Get #intFilenumber, 1, lngNowProgress             '�b����ŏ������܂ꂽ�t�@�C�����猻�݂̌v�Z���Ă���l�𓾂�A���ꂪ�O�̏ꍇ
            If lngNowProgress = -1 Or lngNowProgress = -2 Then              '1���Z���Ă���̂łQ�͐^�A�P�͋U�Ƃ��Ď擾����                                                                 '�v�Z�I��������
               mblnEndFlag = False               '�I���������Ăѕs�ɂ���
               Close #intFilenumber
               muPreserved.blnCulculateComplete = True
               muPreserved.intisPrimeFlag = -1 * (lngNowProgress) - 1 '�l���P���O�ɖ߂�
               If muPreserved.intisPrimeFlag = 1 Then      '�f���������ꍇ�A���[�U�[�ɓ`����
                       work = MsgBox("���ᔭ���I�I�I", vbOKOnly, "Collatz's Anti-Exp����������")
               End If
               Kill App.Path & "\" & "CulculateData"
               Call Connect
               Exit Sub
            Else
               Close #intFilenumber
            End If
Exit Sub
Errorhandler:
         Resume Next
End Sub

Private Function Calculate()        '�b����̂d�w�d��p���Čv�Z���s���֐�
Dim intFilenumber As Integer
Dim strexist As String
    
        Label3.Caption = "2^" & CLng(muPreserved.strMustCulculate) & "-1�𔻒蒆"
        Label3.Left = (Form1.Width - Label3.Width) / 2
        Label4.Caption = muPreserved.intWorkCounts & "��ڂ̃��[�N�ł��F�f�[�^�擾���̏��ʁ@" & muPreserved.intPrize & "��"
        Label4.FontSize = Form1.Height * (72 / 1440) / 22
        Label4.Left = 0
        Label4.Top = 0
        intFilenumber = FreeFile
        strexist = Dir(App.Path & "\" & "EndTeller")            '�I�����鎖��A������EndTeller�̃t�@�C�������łɑ��݂��Ă���ꍇ�폜���Ă���
        If strexist = "EndTeller" Then
            Kill App.Path & "\" & "Endteller"
        End If
        ChDir App.Path
            Call FileCodeOut("CulculateData", xORnumber)
            lngProcessID = Shell(App.Path & "\" & "collatz.exe" & " " & CLng(muPreserved.strMustCulculate) & " " & App.Path, vbHide)       '�b����̃\�t�g���N������
            Timer1.Enabled = True                 '�b����̃\�t�g�̏I�����������܂��t�@�C���̒l���m���߂�^�C�}�[�̋N��
            mblnEndFlag = True
End Function

Private Function Connect()        '�T�[�o�[�ւ̐ڑ����s���֐�
Dim sngStartTime As Single         '�ڑ��^�C���A�E�g���f�̂��߂̎����ێ��p
Dim work As Integer
Dim work2 As Long
        
        On Error GoTo errorhundler
        Timer1.Enabled = False
        If muPreserved.blnAutoConnectConsent = False Then        '�����ڑ��������Ă��Ȃ��ꍇ
                 work = MsgBox("�T�[�o�֐ڑ������܂�����낵���ł��傤���H", vbOKCancel, "Collatz's Anti-Exp����������")
                 If work = vbCancel Then
                        mblnEndFlag = True
                        Label3.Caption = "�ҋ@��"
                        Label3.Left = (Form1.Width - Label3.Width) / 2
                        mnuExecute.Visible = True
                        Exit Function
                 End If
        End If
        Winsock1.RemoteHost = ChangeAddres(SpaceTrim(muPreserved.strAddres))    '�T�[�o�̃R���s���[�^���ݒ�
        If Winsock1.RemoteHost = "" Then         '�Ή�����A�h���X��������Ȃ������ꍇ
                        mblnEndFlag = True
                        Label3.Caption = "�T�[�o�Ɉُ�L��E�ҋ@��"
                        Label3.Left = (Form1.Width - Label3.Width) / 2
                        mnuExecute.Visible = True
        End If
        Label4.Caption = ""
        Label3.Caption = "�T�[�o�ڑ���"
        Label3.Left = (Form1.Width - Label3.Width) / 2
        Winsock1.Close
            Winsock1.Connect
            sngStartTime = Timer
            Do                        '�ڑ���������������܂őҋ@����
               DoEvents
               If Timer - sngStartTime > 30 Then         '30�b�����Ă��ڑ����m���ł��Ȃ��ꍇ�́A�ُ�L��Ƃ��ďI������
                  GoTo errorhundler
                  Exit Do
               End If
               If mblnConnectionFlag = True Then
                  mblnConnectionFlag = False
                  Exit Do
               End If
            Loop
            DoEvents
            Label3.Caption = "�f�[�^���M��"
            Label3.Left = (Form1.Width - Label3.Width) / 2
            work2 = muPreserved.lngWorkingSecond + (Timer - lngProgramStartTime)                '�����ł��o�ߕb���������Ă���
            Timer3.Enabled = True            '�ꕪ�o�ߌ�ł��������Ȃ������ꍇ���̃^�C�}�[�ɂ���ċ����I������
            If muPreserved.blnSpecSendConsent = True Then    '�ŏ��̐ڑ����X�y�b�N�̑��M�������Ă���ꍇ
                        Winsock1.SendData InCode(SpaceTrim(muPreserved.strName) & "!" & SpaceTrim(muPreserved.strMustCulculate) & "!" & muPreserved.intFilenumber & "!" & muPreserved.intisPrimeFlag & "!" & SpaceTrim(muPreserved.strMassageTo) & "!" & GetMyCpuClock() & "!" & getCpuType & "!" & getMemolySize() & "!" & work2, xORnumber) & "#"
            Else
                        Winsock1.SendData InCode(SpaceTrim(muPreserved.strName) & "!" & SpaceTrim(muPreserved.strMustCulculate) & "!" & muPreserved.intFilenumber & "!" & muPreserved.intisPrimeFlag & "!" & SpaceTrim(muPreserved.strMassageTo) & "!" & "0" & "!" & "secret" & "!" & "0" & "!" & work2, xORnumber) & "#"
            End If

Exit Function
errorhundler:
    If mintFoultCount >= 5 Then                    '���łɂT��ڑ������s���Ă��܂��Ă���ꍇ
        work = MsgBox("�T�[�o�[���������Ȃ����ߏI�����܂�", vbOKOnly, "Collatz's Anti-Exp����������")
        Call EndWorking
    Else
        Label3.Caption = "�ڑ����s�E�ҋ@��"
        Label3.Left = (Form1.Width - Label3.Width) / 2
        Timer2.Enabled = True                      '�ꕪ��ɍĂѐڑ������݂�^�C�}�[���N������
        mintFoultCount = mintFoultCount + 1
        mblnEndFlag = True
    End If
End Function

Private Sub Picture1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
      If (Button And vbMiddleButton) = vbMiddleButton Then
        Select Case X \ Screen.TwipsPerPixelX
        Case WM_LBUTTONDOWN
            Call ShowForm
        Case WM_RBUTTONDOWN
'             Me.PopupMenu mnuAllowTime, vbPopupMenuRightButton
        End Select
    End If
End Sub

Private Sub Form_Resize()                    '�ŏ����{�^���������ꂽ�ꍇ�A�c�[���o�[�Ɋi�[����
    If blnFormVisibleFlag = True Then
        blnFormVisibleFlag = False
        Form1.Visible = False
        lRetVal = Shell_NotifyIcon(NIM_ADD, nidSysInfo)
    End If
End Sub

Public Function ShowForm()           '�����ݒ���s���Ȃ���t�H�[����\������
Dim work As Integer
        
        Form1.Visible = True
        Form1.WindowState = vbNormal
        Form1.Caption = "Collatz's Anti-Exp@�g������"
        Form1.Height = Screen.Height * (7 / 10)
        Form1.Width = Screen.Width * (7 / 10)
        Form1.BackColor = vbScrollBars
        Label1.FontSize = Form1.Height * (72 / 1440) / 14                 '�s���������|�C���g�ɕϊ����Ă��當���̑傫���𒲐�
        Label1.Left = (Form1.Width - Label1.Width) / 2
        Label1.Top = Form1.Height * (2 / 10)
        Label5.FontSize = Form1.Height * (72 / 1440) / 14
        Label5.Top = Label1.Top - Label5.Height
        Label5.Left = (Form1.Width - Label1.Width) / 2
        Label2.FontSize = Form1.Height * (72 / 1440) / 10
        Label2.Left = (Form1.Width - Label2.Width) / 2
        Label2.Top = Form1.Height * (3 / 10)
        Label3.FontSize = Form1.Height * (72 / 1440) / 10
        Label3.Top = Form1.Height * (6 / 10)
        Label3.Left = (Form1.Width - Label3.Width) / 2
        work = SetWindowPos(mHwnd, HWND_TOPMOST, 0, 0, 0, _
                0, SWP_SHOWWINDOW Or SWP_NOMOVE Or SWP_NOSIZE)  '�E�B���h�E����Ɏ�O�ɕ\��
        lRetVal = Shell_NotifyIcon(NIM_DELETE, nidSysInfo)
        blnFormVisibleFlag = True      '�t�H�[�����\������Ă��鎖������
End Function
Public Function Savedata()             '���W�X�g���[�t�@�C���Ɍ��݂̏�Ԃ��������ފ֐�
Dim intFilenumber As Integer
    
    Call FileCodeOut("Registry", xORnumber)
    intFilenumber = FreeFile
    Open App.Path & "\" & "Registry" For Random As intFilenumber Len = Len(muPreserved)         '���̎��_�ł̏�����������
        Put #intFilenumber, 1, muPreserved
    Close #intFilenumber
    Call FileCodeIn("Registry", xORnumber)               '�t�@�C�����Í������Ă���
End Function

Public Function LoadData()              '���W�X�g���[�t�@�C������f�[�^��ǂݍ��ފ֐�
Dim intFilenumber As Integer
Dim strexist As String
Dim work As Integer

On Error GoTo errorhundler
    Call FileCodeOut("Registry", xORnumber)               '�t�@�C���𕜍�������
    strexist = Dir(App.Path & "\" & "Registry")       '�t�@�C�������݂���ꍇ
        If strexist = "Registry" Then
            intFilenumber = FreeFile
            Open App.Path & "\" & "Registry" For Random As intFilenumber Len = Len(muPreserved)
                Get #intFilenumber, 1, muPreserved              '���W�X�g���[�t�@�C����������擾
            Close #intFilenumber
        End If
    Call FileCodeIn("Registry", xORnumber)         '�t�@�C�����Í�������
    
Exit Function
errorhundler:
    work = MsgBox("�G���[���������܂����B�����ݒ�����Ȃ����ĉ�����", vbOKOnly, "Collatz's Anti-Exp����������")
    mblnEndFlag = True
    Call mnuPrepare_Click
End Function

Private Function EndWorking()            '�I���������s���֐�
Dim intFilenumber As Integer
Dim strexist As String
Dim ProcesshWnd As Long
Dim ExitCode As Long
Dim ret As Long

    strexist = Dir(App.Path & "\" & "CulculateData")
    If strexist = "CulculateData" Then            '�b�����������������c�����������݂���B�܂�A�b����̃A�v�����v�Z���܂������������A�N�����Ă���ꍇ
            intFilenumber = FreeFile
            Open App.Path & "\" & "EndTeller" For Binary As intFilenumber
            Close intFilenumber
    End If
    muPreserved.lngWorkingSecond = muPreserved.lngWorkingSecond + (Timer - lngProgramStartTime)         '����̃v���O�����N�����Ԃ𑍋N�����Ԃɉ��Z����
    Call Savedata
    If lngProcessID <> 0 Then       '��x�ł��r���������֐���p���Ă����ꍇ
            ProcesshWnd = OpenProcess(PROCESS_QUERY_INFORMATION, 1, lngProcessID)      '�v���Z�X�n���h���𓾂�
            If ProcesshWnd <> 0 Then       '�v���Z�X�n���h��������ꂽ�ꍇ
                Do                                                       '�v���Z�X���N�����̏ꍇ�͑ҋ@
                    Call GetExitCodeProcess(ProcesshWnd, ExitCode)
                    DoEvents
                Loop While (ExitCode = STILL_ACTIVE)
            End If
            Call CloseHandle(ProcesshWnd)   '�v���Z�X�n���h�����������
            Call FileCodeIn("CulculateData", xORnumber)    '�t�@�C�����Í�������
    End If
    If blnFormVisibleFlag = False Then       '�����^�X�N�g���C�ɃA�C�R��������ꍇ��菜��
            lRetVal = Shell_NotifyIcon(NIM_DELETE, nidSysInfo)
    End If
    End
End Function

Private Function SpaceTrim(Data As String) As String        '�����_���A�N�Z�X�ŏ������񂾎��ɖ����ɖ��߂�ꂽ�X�y�[�X����菜���֐�
     SpaceTrim = Trim(Replace(Data, Chr(0), ""))
End Function

Private Function getMemolySize() As Integer
On Error Resume Next
    ChDir App.Path
    getMemolySize = GetMemDat(1) / 1024 / 1024
End Function

Private Function GetMyCpuClock() As Double           '�b�o�t�̃N���b�N����Ԃ�
On Error Resume Next
   '  MHz�P�ʂɂ��邽�߁A1000000(=1M)�Ŋ���
    ChDir App.Path
    GetMyCpuClock = Format(getCpuClock() / 1000000, "0.00")
End Function

Private Function getCpuType() As String
Dim ret As Long
Dim str As String

On Error Resume Next
    ChDir App.Path
    ' DLL���̊֐����Ăяo��
    ret = GetProcessorType()

    Select Case ret
    Case 0
         str = "INTEL 386"
    Case 1
         str = "INTEL 486"
    Case 2
         str = "INTEL PENTIUM"
    Case 3
         str = "MIPS R4000"
    Case 4
         str = "ALPHA 21064"
    Case Else
         str = "Unknown"
    End Select
    getCpuType = str

End Function

Private Function ChangeTime(ByVal Second As Long) As String             '�b����n���ƁA�����ԁA�����A���b�ɒ����āA������Ƃ��ĕԂ��֐�
Dim Hour As Long
Dim Minute As Long
Dim work As Long
Dim str As String

            Hour = Second \ 3600
            work = Second Mod 3600
            Minute = work \ 60
            Second = work Mod 60
            If Hour <> 0 Then
                  str = Hour & "����"
            End If
            If Minute <> 0 Then
                  str = str & Minute & "��"
            End If
            If Second <> 0 Then
                  str = str & Second & "�b"
            End If
            ChangeTime = str
End Function

Private Function InCode(ByVal str As String, xORnumber As Integer) As String           '�������n���ƈÍ������ꂽ�������Ԃ��i�w���������Ō�������l���K�v�j
Dim bytArray() As Byte
Dim i As Long
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

            xORnumber2 = xORnumber \ 2 + 1        '�Q�x�ڂ�Xor�����ŗ��p����l
            xORnumber3 = xORnumber \ 3 + 2
            ReDim bytArray(lstrlen(str))
            Call MoveMemory(bytArray(0), ByVal str, lstrlen(str))
            str = ""
            For i = 0 To UBound(bytArray) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber)
            Next i
            For i = 1 To UBound(bytArray) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber2)
            Next i
            For i = 0 To UBound(bytArray)
                bytArray(i) = CByte(bytArray(i) Xor xORnumber3)
                str = str & "," & bytArray(i)
            Next i
                str = Right(str, Len(str) - 1)
                InCode = str
End Function

Private Function OutCode(ByVal str As String, xORnumber As Integer)                 '�Í������ꂽ�������n���ƕ��������ĕԂ��i�w���������Ō��������l���K�v�j
Dim bytArray() As Byte
Dim valsplited As Variant
Dim i As Long
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

            xORnumber2 = xORnumber \ 2 + 1        '�Q�x�ڂ�Xor�����ŗ��p����l
            xORnumber3 = xORnumber \ 3 + 2
            valsplited = Split(str, ",")
            ReDim bytArray(UBound(valsplited))
            For i = 0 To UBound(valsplited)
                bytArray(i) = CByte(valsplited(i))
                bytArray(i) = CByte(bytArray(i) Xor xORnumber3)
            Next i
            For i = 0 To UBound(valsplited) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber)
            Next i
            For i = 1 To UBound(valsplited) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber2)
            Next i
            OutCode = StrConv(bytArray, vbUnicode)
End Function

Public Function FileCodeIn(FileNum As String, xORnumber As Integer)              '�^�����t�@�C���̈Í���������@���@�t�@�C�����Ɋg���q���Ȃ��A�܂��A�t�@�C�����݂̂̂��́A�{���t���Ă��Ȃ����́i�w���������Ō�������l���K�v�j
Dim Dat() As Byte                '�t�@�C���̃f�[�^
Dim i As Long, Lf As Long      '�J�E���^�ƃt�@�C���T�C�Y
Dim intFilenumber As Integer                '�t�@�C���i���o�[
Dim strexist As String
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

On Error GoTo errorhundler
        strexist = Dir(App.Path & "\" & FileNum)       '�Í�������Ă��Ȃ��t�@�C�������݂���ꍇ
        If strexist = FileNum Then
            xORnumber2 = xORnumber \ 2 + 1        '�Q�x�ڂ�Xor�����ŗ��p����l
            xORnumber3 = xORnumber \ 3 + 2
            '�Í�������t�@�C���̃f�[�^��ǂݍ���
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum For Binary As intFilenumber
                   Lf = LOF(intFilenumber)
                   ReDim Dat(Lf)
                   Get #intFilenumber, 1, Dat
            Close intFilenumber
            
            '�Í�������
            For i = 0 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber)
            Next i
            For i = 1 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber2)
            Next i
            For i = 1 To Lf
                Dat(i) = CByte(Dat(i) Xor xORnumber3)
            Next i
               
            '�Í��������f�[�^���t�@�C���ɏ�������
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum & "�{" For Binary As intFilenumber
            Put #intFilenumber, 1, Dat
            Close intFilenumber
            
            Kill App.Path & "\" & FileNum        '���̃t�@�C�����폜���Ă���
        End If
Exit Function
errorhundler:
Resume
                    
End Function

Public Function FileCodeOut(FileNum As String, xORnumber As Integer)              '�^�����t�@�C���̕�����������@���t�@�C�����Ɋg���q���Ȃ��A�܂��A�t�@�C�����̂݁i�w���������Ō��������l���K�v�j
Dim Dat() As Byte                '�t�@�C���̃f�[�^
Dim i As Long, Lf As Long      '�J�E���^�ƃt�@�C���T�C�Y
Dim intFilenumber As Integer                '�t�@�C���i���o�[
Dim strexist As String
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer
       
       strexist = Dir(App.Path & "\" & FileNum & "�{")        '�t�@�C�������݂���ꍇ
       If strexist = FileNum & "�{" Then            '�t�@�C�����ɈÍ�������Ă��鎖�������{���t���Ă�����̂����݂���ꍇ
            xORnumber2 = xORnumber \ 2 + 1        '�Q�x�ڂ�Xor�����ŗ��p����l
            xORnumber3 = xORnumber \ 3 + 2
            '����������t�@�C���̃f�[�^��ǂݍ���
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum & "�{" For Binary As intFilenumber
                   Lf = LOF(intFilenumber)
                   ReDim Dat(Lf)
                   Get #intFilenumber, 1, Dat
            Close intFilenumber
            
            '����������
            For i = 1 To Lf
                Dat(i) = CByte(Dat(i) Xor xORnumber3)
            Next i
            For i = 0 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber)
            Next i
            For i = 1 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber2)
            Next i
            
            '�����������f�[�^���t�@�C���ɏ�������
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum For Binary As intFilenumber
            Put #intFilenumber, 1, Dat
            Close intFilenumber
                       
            Kill App.Path & "\" & FileNum & "�{"                  '���̃t�@�C�����폜���Ă���
       End If
End Function

Private Function ChangePriorityC(PriorityClass As Long)        '�b����̂d�w�d���N������Ă���ꍇ�A�D��x��ύX����֐��@�����͗D��x�������N���X
Dim ProcesshWnd As Long
            
    If lngProcessID <> 0 Then       '��x�ł��r���������֐���p���Ă����ꍇ
            ProcesshWnd = OpenProcess(PROCESS_QUERY_INFORMATION, 1, lngProcessID)      '�v���Z�X�n���h���𓾂�
            If ProcesshWnd <> 0 Then       '�v���Z�X�n���h��������ꂽ�ꍇ
                    Call SetPriorityClass(ProcesshWnd, PriorityClass)           '�v���Z�X�̗D��x��ύX����
            End If
            Call CloseHandle(ProcesshWnd)   '�v���Z�X�n���h�����������
    End If
End Function

Private Function ChangeAddres(Addres As String) As String         '�^����ꂽ�h���C�����h�o�A�h���X�ɕϊ�����֐��@�����̓h���C���@�߂�l�͂h�o�A�h���X �h�o�A�h���X��n�����ꍇ�͂��̂܂ܕԂ�
Dim ChangeClass As ws            '�N���X����������ϐ�
Dim work As String
    On Error Resume Next
        Set ChangeClass = New ws         '�N���X�����[�h����
        work = ChangeClass.GetIP(Addres) '�z�X�g��:xxx��IP�A�h���X��\��
        Set ChangeClass = Nothing        '�N���X���������
        ChangeAddres = work
End Function








[module1.bas]



Option Explicit

Public Const xORnumber = 173       '�Í����ɂ����Č�������l

Public Type Preserved
    strName As String * 100                '���[�U�[���ێ��p
    strAddres As String * 100              '�T�[�o�[�̃A�h���X�ێ��p
    strMustCulculate As String * 100            '�v�Z���ׂ��l��ێ�
    intFilenumber As Integer              '�T�[�o���ۑ����Ă���N���C�A���g�f�[�^�t�@�C���̂ǂ��Ɏ����̃f�[�^���ێ�����Ă��邩
    lngNowProgress As Long             '���݂ǂ��܂Ōv�Z���I�����Ă��邩
    intisPrimeFlag As Integer              '���茋�ʂ��f���ł��������A�f���ł͂Ȃ������� 1=�^�@0=�U
    intWorkCounts As Integer               '���̃N���C�A���g���ǂꂾ���̃��[�N�������Ȃ�����
    strMassageTo As String * 1000               '�T�[�o�[�ւ̃��b�Z�[�W
    blnAutoConnectConsent As Boolean       '�����ڑ���F�߂邩�ǂ���
    blnSpecSendConsent As Boolean         '�p�\�R���̃X�y�b�N���𑗐M���Ă��悢��
    blnCulculateComplete As Boolean        '�v�Z�͏I�����Ă��邩
    intPrize As Integer                    '�T�[�o�[�֐ڑ����s�������_�ł̏���
    lngWorkingSecond As Long             '�v���O�����̑��N�����Ԃ�b���ŕێ�
    blnAutoStartFlag As Boolean           '�v���O�������N��������A�����ŏ������n�߂Ă悢��
End Type

Declare Function lstrlen Lib "KERNEL32" Alias "lstrlenA" (ByVal lpString As String) As Long    ' lstrlen()�֐��@������̃o�C�g���𓾂�`�o�h�̐錾
Declare Sub MoveMemory Lib "KERNEL32" Alias "RtlMoveMemory" (pDest As Any, pSource As Any, ByVal ByteLen As Long)      ' ����������֐��@��������o�C�g�z��ɑ������

