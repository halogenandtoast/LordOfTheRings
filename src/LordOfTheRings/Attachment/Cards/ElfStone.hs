module LordOfTheRings.Attachment.Cards.ElfStone (elfStone) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Classes.HasQueue (insertAfterMatching)

newtype ElfStone = ElfStone AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to the active location.
elfStone :: AttachmentCard ElfStone
elfStone = attachment ElfStone Cards.elfStone & attachTo (AttachToLocation IsActiveLocation)

-- | Attached location gets +1 quest point.
instance HasModifiersFor ElfStone where
  getModifiersFor (ElfStone a) = withHost a \host -> modified a host [QuestPointModifier 1]

{- | Response: After attached location leaves play as an explored location, the first
player puts 1 ally into play from his hand.

Elf-stone leaves play with the location, so it reads the moment as it is explored and
offers its response once the location is gone: right after the windows that answer it
having been explored.
-}
instance HasAbilities ElfStone where
  getAbilities (ElfStone a) = [mkAbility a 1 $ silent $ ExploredWindow When lid | AttachedTo (LocationTarget lid) <- [a.placement]]

instance RunMessage ElfStone where
  runMessage msg s@(ElfStone a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [lid | Window _ (Explored lid) <- windows] \lid -> do
        lead <- getLead
        allies <- filter ((== AllyType) . (.cardType)) . (.hand) <$> getPlayer lead
        use <- capture $ chooseOneM a.controller do
          questionLabeled "elfStone.use"
          causedBy a
          labeled "elfStone.putIntoPlay" $ chooseOrRunOneM lead do
            questionLabeled "elfStone.ally"
            causedBy a
            targets allies \c -> putAllyIntoPlayFromHand lead c.id
          labeled "elfStone.skip" (pure ())
        lift $ insertAfterMatching use \case
          CheckWindows ws -> any (\case Window After (Explored lid') -> lid' == lid; _ -> False) ws
          _ -> False
      pure s
    _ -> ElfStone <$> runMessage msg a
