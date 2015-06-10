  static void RegisterFunction(Type reg1; Type2 reg2)
  {
    CategoryRegistration(registerType);
  }

  [ComUnregisterFunction()]
  [ComVisible(false)] 
  static void UnregisterFunction(Type reg1; Type[] regs; Type2 reg2)
  {
    CategoryUnregistration(registerType);
  }
