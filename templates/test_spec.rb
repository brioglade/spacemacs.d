#!/usr/bin/env spec

require './lib/test_${1:`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`}.rb'

describe $1 do
  describe '.${2:some_method}' do
    context 'given an ${3:some input}' do
      it 'returns ${4:something}' do
        expect($1.$2('$3')).to eql('$4')
      end
    end
  end

end
