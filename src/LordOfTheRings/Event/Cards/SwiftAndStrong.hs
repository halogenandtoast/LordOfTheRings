module LordOfTheRings.Event.Cards.SwiftAndStrong (swiftAndStrong) where

import LordOfTheRings.Attachment.Types (attachmentExhausted)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Weapon))

newtype SwiftAndStrong = SwiftAndStrong EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a character you control attacks and destroys an enemy, exhaust a
-- Weapon attached to that character to ready that character and give it +2 [attack] for
-- its next attack this phase.
swiftAndStrong :: EventCard SwiftAndStrong
swiftAndStrong =
  eventWith SwiftAndStrong Cards.swiftAndStrong \a ->
    let you = PlayerWithId a.owner
     in a
          & windowL
          ?~ EnemyDestroyedByCharacterWindow
            After
            (HeroControlledBy you <> HeroWithReadyAttached Weapon)
            (AllyControlledBy you <> AllyWithReadyAttached (Just Weapon))

instance RunMessage SwiftAndStrong where
  runMessage msg e@(SwiftAndStrong attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      mine <- (<>) <$> (map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)) <*> (map toTarget <$> select (AllyControlledBy $ PlayerWithId pid))
      let characters = ordNub [c | Window _ (EnemyDestroyedByAttack _ attackers _) <- windows, c <- attackers, c `elem` mine]
      choices <- concatForM characters \c -> do
        weapons <- attachedWithTrait Weapon c
        ready' <- flip filterM weapons \case
          AttachmentTarget aid -> maybe False (not . attachmentExhausted . toAttrs) . lookup aid <$> getsGame (.entities.attachments)
          _ -> pure False
        pure [(c, w) | w <- ready']
      chooseOrRunOneM pid do
        questionLabeled "swiftAndStrong.weapon"
        causedBy attrs
        for_ choices \(c, w) -> targeting w do
          exhaust w
          ready c
          phaseModifier attrs c (ForNextAttack $ AttackModifier 2)
      pure e
    _ -> SwiftAndStrong <$> runMessage msg attrs
