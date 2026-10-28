module LordOfTheRings.Treachery.Cards.OrcAssault (orcAssault) where

import LordOfTheRings.Trait (Trait (Battleground))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OrcAssault = OrcAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcAssault :: TreacheryCard OrcAssault
orcAssault = treachery OrcAssault Cards.orcAssault

-- | When Revealed: Each character gets -2 attack and -2 defense until the end of the round.
-- Shadow: Deal 2 damage to all Battleground locations in play.
instance RunMessage OrcAssault where
  runMessage msg t@(OrcAssault attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> roundModifiers attrs c [AttackModifier (-2), DefenseModifier (-2)]
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      battlegrounds <- select (LocationWithTrait Battleground)
      for_ battlegrounds \l -> dealDamage attrs l 2
      pure t
    _ -> OrcAssault <$> runMessage msg attrs
