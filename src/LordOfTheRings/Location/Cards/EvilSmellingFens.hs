module LordOfTheRings.Location.Cards.EvilSmellingFens (evilSmellingFens) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype EvilSmellingFens = EvilSmellingFens LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

evilSmellingFens :: LocationCard EvilSmellingFens
evilSmellingFens = location EvilSmellingFens Cards.evilSmellingFens

-- | Mire 3.
-- Forced: When Evil-smelling Fens is discarded by the Mire keyword, deal 1 damage to each
-- character.
instance HasAbilities EvilSmellingFens where
  getAbilities (EvilSmellingFens a) = [mkAbility a 1 $ forced $ DiscardedByMireWindow When a.id]

-- | Travel: Raise each player's threat by 1 to travel here.
-- Shadow: Deal 1 damage to defending character.
instance RunMessage EvilSmellingFens where
  runMessage msg l@(EvilSmellingFens a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \pid -> encounterRaisesThreat pid 1
      EvilSmellingFens <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> dealDamage a c 1
      pure l
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \d -> dealDamage a d 1
      pure l
    _ -> EvilSmellingFens <$> runMessage msg a
