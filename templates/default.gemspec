# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)

Gem::Specification.new do |spec|
  spec.name          = '${1:`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`}'
  spec.version       = '1.0'
  spec.authors       = ['Howard Abrams']
  spec.email         = ['howard.abrams@gmail.com']
  spec.summary       = %q{$2}
  spec.description   = %q{$3}
  spec.homepage      = 'http://${4:domainforproject.com}/'
  spec.license       = 'MIT'

  spec.files         = ['lib/$1.rb']
  spec.executables   = ['bin/$1']
  spec.test_files    = ['spec/test_$1.rb']
  spec.require_paths = ['lib']
end
