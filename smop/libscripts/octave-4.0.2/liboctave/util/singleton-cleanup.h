#if !defined (octave_singleton_cleanup_h)
#define octave_singleton_cleanup_h 1

#include <set>

class
OCTAVE_API
singleton_cleanup_list
{
protected:

  singleton_cleanup_list (void) : fcn_list () { }

public:

  typedef void (*fptr) (void);

  ~singleton_cleanup_list (void);

  static void add (fptr f)
  {
    if (instance_ok ())
      instance->do_add (f);
  }

  static void cleanup (void) { delete instance; instance = 0; }

private:

  static singleton_cleanup_list *instance;

  static bool instance_ok (void);

  static void cleanup_instance (void) { delete instance; instance = 0; }

  std::set<fptr> fcn_list;

  void do_add (fptr f)
  {
    fcn_list.insert (f);
  }

  // No copying!

  singleton_cleanup_list (const singleton_cleanup_list&);

  singleton_cleanup_list& operator = (const singleton_cleanup_list&);
};

#endif
