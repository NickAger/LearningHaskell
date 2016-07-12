-- Chat.hs
-- Copyright (c) 2013 Well-Typed LLP

-- Exercise CH1.
--
-- Define a chat server that listens on a port for incoming connections. 
--
-- Communication is via a line-based textual protocol.
--
-- Clients can connect and send messages. Messages are broadcast to all other
-- connected clients.
--
-- Use STM for handling shared state.
--
-- Define a simple client, too. Make one program behave as client or server on
-- demand, depending on command-line flags.

-- Exercise CH2.
--
-- Handle exceptions properly in the server so that disconnecting
-- clients do not cause trouble.

-- Exercise CH3.
--
-- Before entering the main "chat loop", every client should first specify
-- a nickname. The server keeps track of nicknames. It will respond either
-- "True" or "False" to the nickname sent. If "True" is sent, the nickname
-- is accepted and the chat loop is entered. If "False" is sent, the
-- nickname is not accepted (because it is already taken), and the client
-- is expected to send another nickname until one is accepted.
--
-- When messages are broadcast, the server will always add an indication of
-- who sent the message.

-- Exercise CH4.
--
-- Make the client more comfortable to use by means of the haskeline
-- library.

-- Further ideas.
--
-- - Extend the protocol so that client-messages starting with a slash '/'
-- are interpreted as commands. Add a command that allows to change the
-- nickname to another nickname, and a command that queries who's currently
-- connected.
--
-- - Make the server send status messages whenever someone joins and
-- whenever someone leaves.
--
-- - Make it possible to send private messages to a specific nick.
--
-- - Integrate the calculator into the chat service.

