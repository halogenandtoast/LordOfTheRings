module LordOfTheRings.Event.Cards.FoeHammer (foeHammer) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Types (attachmentExhausted)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Weapon))

newtype FoeHammer = FoeHammer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a hero you control attacks and destroys an enemy, exhaust a Weapon
-- card attached to that hero to draw 3 cards.
foeHammer :: EventCard FoeHammer
foeHammer =
  eventWith FoeHammer Cards.foeHammer \a ->
    a & windowL ?~ EnemyDestroyedByHeroWindow After (HeroControlledBy (PlayerWithId a.owner) <> HeroWithReadyAttached Weapon)

instance RunMessage FoeHammer where
  runMessage msg e@(FoeHammer attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      mine <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
      let heroes = [h | Window _ (EnemyDestroyedByAttack _ attackers) <- windows, h <- attackers, h `elem` mine]
      weapons <- concatMapM (attachedWithTrait Weapon) heroes
      readyWeapons <- flip filterM weapons \case
        AttachmentTarget aid -> not . attachmentExhausted . toAttrs <$> getsGame (fromJustNote "no attachment" . lookup aid . (.entities.attachments))
        _ -> pure False
      chooseOrRunOneM pid do
        questionLabeled "foeHammer.choose"
        causedBy attrs
        targets readyWeapons \w -> do
          exhaust w
          drawCards pid 3
      pure e
    _ -> FoeHammer <$> runMessage msg attrs
