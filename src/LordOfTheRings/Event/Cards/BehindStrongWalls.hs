module LordOfTheRings.Event.Cards.BehindStrongWalls (behindStrongWalls) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Trait (Trait (Gondor))

newtype BehindStrongWalls = BehindStrongWalls EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Ready a defending Gondor character. That character gets +1 defense until the
-- end of the phase.
--
-- ponytail: an attack has no action window of its own, so this is played in the window
-- after a defender is declared, and offered for any defender; it does nothing to one
-- without the Gondor trait.
behindStrongWalls :: EventCard BehindStrongWalls
behindStrongWalls = eventWith BehindStrongWalls Cards.behindStrongWalls (windowL ?~ DefenderDeclaredWindow After Nothing)

instance RunMessage BehindStrongWalls where
  runMessage msg e@(BehindStrongWalls attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      defenders <- flip filterM [d | Window _ (DeclaredAsDefender d _) <- windows] \d ->
        hasTrait Gondor d =<< characterCardDef d
      chooseOrRunOneM pid do
        questionLabeled "behindStrongWalls.choose"
        causedBy attrs
        targets defenders \d -> do
          ready d
          phaseModifier attrs d (DefenseModifier 1)
      pure e
    _ -> BehindStrongWalls <$> runMessage msg attrs
