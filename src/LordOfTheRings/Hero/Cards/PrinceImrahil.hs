module LordOfTheRings.Hero.Cards.PrinceImrahil (princeImrahil) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype PrinceImrahil = PrinceImrahil HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

princeImrahil :: HeroCard PrinceImrahil
princeImrahil = hero PrinceImrahil Cards.princeImrahil

-- | Response: After a character leaves play, ready Prince Imrahil. (Limit once per round.)
instance HasAbilities PrinceImrahil where
  getAbilities (PrinceImrahil a) =
    [ limited (PlayerLimit PerRound 1)
        $ restricted a 1 (exists $ HeroWithId a.id <> ExhaustedHero)
        $ response (CharacterLeftPlayWindow After AnyCard)
    ]

instance RunMessage PrinceImrahil where
  runMessage msg h@(PrinceImrahil attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      pure h
    _ -> PrinceImrahil <$> runMessage msg attrs
