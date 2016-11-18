/*

Copyright (C) 2007, 2013 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_ov_java_h)
#define octave_ov_java_h 1

#include <jni.h>

#include <oct-obj.h>
#include <ov.h>

template <class T>
class java_local_ref
{
public:

  java_local_ref (JNIEnv *_env)
    : jobj (0), detached (false), env (_env) { }

  java_local_ref (JNIEnv *_env, T obj)
    : jobj (obj), detached (false), env (_env) { }

  ~java_local_ref (void) { release (); }

  T& operator= (T obj)
  {
    release ();
    jobj = obj;
    detached = false;
    return jobj;
  }

  operator bool () const { return (jobj != 0); }
  operator T () { return jobj; }

  void detach (void) { detached = true; }

private:

  void release (void)
  {
    if (env && jobj && ! detached)
      env->DeleteLocalRef (jobj);

    jobj = 0;
  }

  java_local_ref (void)
    : jobj (0), detached (false), env (0)
  { }

protected:
  T jobj;
  bool detached;
  JNIEnv *env;
};

typedef java_local_ref<jobject> jobject_ref;
typedef java_local_ref<jclass> jclass_ref;
typedef java_local_ref<jstring> jstring_ref;
typedef java_local_ref<jobjectArray> jobjectArray_ref;
typedef java_local_ref<jintArray> jintArray_ref;
typedef java_local_ref<jbyteArray> jbyteArray_ref;
typedef java_local_ref<jdoubleArray> jdoubleArray_ref;
typedef java_local_ref<jthrowable> jthrowable_ref;

extern OCTINTERP_API std::string
jstring_to_string (JNIEnv* jni_env, jstring s);

extern OCTINTERP_API std::string
jstring_to_string (JNIEnv* jni_env, jobject obj);

extern OCTINTERP_API octave_value
box (JNIEnv* jni_env, jobject jobj, jclass jcls = 0);

extern OCTINTERP_API octave_value
box_more (JNIEnv* jni_env, jobject jobj, jclass jcls = 0);

extern OCTINTERP_API bool
unbox (JNIEnv* jni_env, const octave_value& val, jobject_ref& jobj,
       jclass_ref& jcls);

extern OCTINTERP_API bool
unbox (JNIEnv* jni_env, const octave_value_list& args,
       jobjectArray_ref& jobjs, jobjectArray_ref& jclss);

extern OCTINTERP_API bool Vjava_matrix_autoconversion;

extern OCTINTERP_API bool Vjava_unsigned_autoconversion;

extern OCTINTERP_API bool Vdebug_java;

class OCTINTERP_API octave_java : public octave_base_value
{
public:

  octave_java (void)
    : octave_base_value (), java_object (0), java_class (0)
  { }

  octave_java (const octave_java& jobj)
    : octave_base_value (jobj), java_object (0), java_class (0)
  {
    init (jobj.java_object, jobj.java_class);
  }

  octave_java (const jobject& obj, jclass cls = 0)
    : octave_base_value (), java_object (0)
  {
    init (obj, cls);
  }

  ~octave_java (void) { release (); }

  jobject to_java (void) const { return java_object; }
  jclass to_class (void) const { return java_class; }

  std::string java_class_name (void) const { return java_classname; }

  octave_base_value* clone (void) const { return new octave_java (*this); }
  octave_base_value* empty_clone (void) const { return new octave_java (); }

  bool is_instance_of (const std::string&) const;

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_map (void) const { return false; }

  bool is_java (void) const { return true; }

  string_vector map_keys (void) const;

  dim_vector dims (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  octave_value_list
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx, int nargout);

  octave_value
  subsref (const std::string& type, const std::list<octave_value_list>& idx)
  {
    octave_value_list retval = subsref (type, idx, 1);
    return (retval.length () > 0 ? retval(0) : octave_value ());
  }

  octave_value subsasgn (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         const octave_value& rhs);

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  bool is_java_string (void) const;

  static JNIEnv* thread_jni_env (void);

  octave_value do_javaMethod (JNIEnv* jni_env, const std::string& name,
                              const octave_value_list& args);

  octave_value
  do_javaMethod (const std::string& name, const octave_value_list& args)
  {
    return do_javaMethod(thread_jni_env (), name, args);
  }

  static octave_value
  do_javaMethod (JNIEnv* jni_env, const std::string& class_name,
                 const std::string& name, const octave_value_list& args);

  static octave_value
  do_javaMethod (const std::string& class_name, const std::string& name,
                 const octave_value_list& args)
  {
    return do_javaMethod(thread_jni_env (), class_name, name, args);
  }

  static octave_value
  do_javaObject (JNIEnv* jni_env, const std::string& name,
                 const octave_value_list& args);

  static octave_value
  do_javaObject (const std::string& name, const octave_value_list& args)
  {
    return do_javaObject (thread_jni_env (), name, args);
  }

  octave_value do_java_get (JNIEnv* jni_env, const std::string& name);

  octave_value do_java_get (const std::string& name)
  {
    return do_java_get (thread_jni_env (), name);
  }

  static octave_value
  do_java_get (JNIEnv* jni_env, const std::string& class_name,
               const std::string& name);

  static octave_value
  do_java_get (const std::string& class_name, const std::string& name)
  {
    return do_java_get (thread_jni_env (), class_name, name);
  }

  octave_value do_java_set (JNIEnv* jni_env, const std::string& name,
                            const octave_value& val);

  octave_value do_java_set (const std::string& name, const octave_value& val)
  {
    return do_java_set (thread_jni_env (), name, val);
  }

  static octave_value
  do_java_set (JNIEnv* jni_env, const std::string& class_name,
               const std::string& name, const octave_value& val);

  static octave_value
  do_java_set (const std::string& class_name, const std::string& name,
               const octave_value& val)
  {
    return do_java_set (thread_jni_env (), class_name, name, val);
  }

private:

  void init (jobject jobj, jclass jcls)
  {
    JNIEnv *current_env = thread_jni_env ();

    if (current_env)
      {
        if (jobj)
          java_object = current_env->NewGlobalRef (jobj);

        if (jcls)
          java_class = reinterpret_cast<jclass> (current_env->NewGlobalRef (jcls));
        else if (java_object)
          {
            jclass_ref ocls (current_env, current_env->GetObjectClass (java_object));
            java_class = reinterpret_cast<jclass> (current_env->NewGlobalRef (jclass (ocls)));
          }

        if (java_class)
          {
            jclass_ref clsCls (current_env, current_env->GetObjectClass (java_class));
            jmethodID mID = current_env->GetMethodID (clsCls, "getCanonicalName", "()Ljava/lang/String;");
            jobject_ref resObj (current_env, current_env->CallObjectMethod (java_class, mID));
            java_classname = jstring_to_string (current_env, resObj);
          }
      }
  }

  void release (void)
  {
    JNIEnv *current_env = thread_jni_env ();

    if (current_env)
      {
        if (java_object)
          current_env->DeleteGlobalRef (java_object);

        if (java_class)
          current_env->DeleteGlobalRef (java_class);

        java_object = 0;
        java_class = 0;
      }
  }

private:

  jobject java_object;

  jclass java_class;

  std::string java_classname;


public:
  int type_id (void) const { return t_id; }
  std::string type_name (void) const { return t_name; }
  std::string class_name (void) const { return java_classname; }

  static int static_type_id (void) { return t_id; }
  static std::string static_type_name (void) { return t_name; }
  static std::string static_class_name (void) { return "<unknown>"; }
  static void register_type (void);

private:
  static int t_id;
  static const std::string t_name;
};

#endif
