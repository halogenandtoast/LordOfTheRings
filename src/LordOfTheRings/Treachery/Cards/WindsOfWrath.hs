module LordOfTheRings.Treachery.Cards.WindsOfWrath (windsOfWrath) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WindsOfWrath = WindsOfWrath TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

windsOfWrath :: TreacheryCard WindsOfWrath
windsOfWrath = treachery WindsOfWrath Cards.windsOfWrath

-- | If your heading is at the worst setting, this effect cannot be canceled.
instance HasModifiersFor WindsOfWrath where
  getModifiersFor (WindsOfWrath a) = whenM atWorstHeading $ modified a a [WhenRevealedCannotBeCanceled]

-- | When Revealed: If you are off-course, deal 1 damage to each character in play.
-- Shadow: Either shift your heading off-course or treat this attack as undefended. An
-- undefended attack already is, so it asks nothing.
instance RunMessage WindsOfWrath where
  runMessage msg t@(WindsOfWrath attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      whenM isOffCourse do
        characters <- selectCharacters AnyHero AnyAlly
        for_ characters \c -> dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      unless (null defenders) do
        chooseOneM pid do
          questionLabeled "windsOfWrath.choose"
          causedBy attrs
          labeled "shiftHeadingOffCourse" $ shiftHeadingOffCourse attrs
          -- the attack's own messages are on the game's queue once the choice is made
          labeled "windsOfWrath.undefended" $ doStep 1 msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      makeUndefended pid eid
      pure t
    _ -> WindsOfWrath <$> runMessage msg attrs
