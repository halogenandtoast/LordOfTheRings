module LordOfTheRings.Attachment.Cards.SongOfMocking (songOfMocking) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype SongOfMocking = SongOfMocking AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

songOfMocking :: AttachmentCard SongOfMocking
songOfMocking = attachment SongOfMocking Cards.songOfMocking

-- | Action: Exhaust Song of Mocking to choose another hero. Until the end of the phase,
-- attached hero takes all damage assigned to the chosen hero.
instance HasAbilities SongOfMocking where
  getAbilities (SongOfMocking a) =
    [ restricted a 1 (exists $ not_ $ HeroWithId hid) $ ActionAbility (exhaustCost a)
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage SongOfMocking where
  runMessage msg s@(SongOfMocking a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      withHost a \case
        host@(HeroTarget hid) -> do
          others <- select (not_ $ HeroWithId hid)
          chooseOneM pid do
            questionLabeled "songOfMocking.chooseHero"
            causedBy a
            targets others \h -> phaseModifier a h (DamageRedirectedTo host)
        _ -> pure ()
      pure s
    _ -> SongOfMocking <$> runMessage msg a
