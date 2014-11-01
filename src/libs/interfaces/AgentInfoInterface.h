
/***************************************************************************
 *  AgentInfoInterface.h - Fawkes BlackBoard Interface - AgentInfoInterface
 *
 *  Templated created:   Thu Oct 12 10:49:19 2006
 *  Copyright  2014  Tim Niemueller
 *
 ****************************************************************************/

/*  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version. A runtime exception applies to
 *  this software (see LICENSE.GPL_WRE file mentioned below for details).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  Read the full text in the LICENSE.GPL_WRE file in the doc directory.
 */

#ifndef __INTERFACES_AGENTINFOINTERFACE_H_
#define __INTERFACES_AGENTINFOINTERFACE_H_

#include <interface/interface.h>
#include <interface/message.h>
#include <interface/field_iterator.h>

namespace fawkes {

class AgentInfoInterface : public Interface
{
 /// @cond INTERNALS
 INTERFACE_MGMT_FRIENDS(AgentInfoInterface)
 /// @endcond
 public:
  /* constants */

 private:
#pragma pack(push,4)
  /** Internal data storage, do NOT modify! */
  typedef struct {
    int64_t timestamp_sec;  /**< Interface Unix timestamp, seconds */
    int64_t timestamp_usec; /**< Interface Unix timestamp, micro-seconds */
    char message[256]; /**< Info message. */
  } AgentInfoInterface_data_t;
#pragma pack(pop)

  AgentInfoInterface_data_t *data;

 public:
  /* messages */
  virtual bool message_valid(const Message *message) const;
 private:
  AgentInfoInterface();
  ~AgentInfoInterface();

 public:
  /* Methods */
  char * message() const;
  void set_message(const char * new_message);
  size_t maxlenof_message() const;
  virtual Message * create_message(const char *type) const;

  virtual void copy_values(const Interface *other);
  virtual const char * enum_tostring(const char *enumtype, int val) const;

};

} // end namespace fawkes

#endif
