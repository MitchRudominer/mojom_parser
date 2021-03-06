// Copyright 2015 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

package lexer

type TokenStream interface {
	// Returns the next Token in the stream without advancing the cursor,
	// or returns the EOF token if the cursor is already past the end.
	PeekNext() Token

	// Advances the cursor in the stream or does nothing if the cursor is
	// already past the end of the stream
	ConsumeNext()
}

func EOFToken() Token {
	eof := Token{Kind: EOF}
	return eof
}

// *TokenChan implements TokenStream.
type TokenChan struct {
	tokenChan chan Token
	nextToken Token
	buffered  bool
}

func (s *TokenChan) PeekNext() (token Token) {
	if !s.buffered {
		s.buffered = true
		s.ConsumeNext()
	}

	return s.nextToken
}

func (s *TokenChan) ConsumeNext() {
	if t, open := <-s.tokenChan; open {
		s.nextToken = t
	} else {
		s.nextToken = EOFToken()
	}
}

// *TokenSlice implements TokenStream
type TokenSlice []Token

func (slice *TokenSlice) PeekNext() (token Token) {
	if len(*(slice)) == 0 {
		token = EOFToken()
		return
	}
	token = (*slice)[0]
	return
}

func (slice *TokenSlice) ConsumeNext() {
	if len(*(slice)) == 0 {
		return
	}
	(*slice) = (*slice)[1:]
}
