module LordOfTheRings.Treachery.Cards.VeryWeary (veryWeary) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype VeryWeary = VeryWeary TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

veryWeary :: TreacheryCard VeryWeary
veryWeary = treachery VeryWeary Cards.veryWeary

instance RunMessage VeryWeary where
  runMessage msg t@(VeryWeary a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> do
        questing <- selectCharacters
          (CommittedHero <> HeroControlledBy (PlayerWithId pid))
          (CommittedAlly <> AllyControlledBy (PlayerWithId pid))
        chooseOrRunOneM pid do
          questionLabeled "veryWeary.damage"
          causedBy a
          targets questing (\c -> dealDamage a c 1)
      phaseModifier a GameTarget (EachDamagedCharacter $ WillpowerModifier (-2))
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders (\c -> dealDamage a c 1)
      pure t
    _ -> VeryWeary <$> runMessage msg a
